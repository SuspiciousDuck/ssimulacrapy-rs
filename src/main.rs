mod metrics;
mod progress;
mod scores;
mod utils;
use clap::Parser;
use indicatif::{MultiProgress, ProgressBar, ProgressDrawTarget};
use macros::{exists, lossy_to_string};
use metrics::Metrics;
use progress::{init_bar, mk_bar};
use scores::{FrameMetrics, MetricStats, Scores};
use std::collections::BTreeMap;
use std::thread::available_parallelism as threads;
use std::{
    collections::HashMap,
    num::{NonZero, NonZeroUsize},
    path::PathBuf,
    process::exit,
    sync::mpsc::channel,
};
use utils::{b2sum, calculate_scores, method_to_metric, mk_graph, score_from_metrics};

mod macros {
    macro_rules! lossy_to_string {
        ($expr:expr) => {
            $expr.to_string_lossy().to_string()
        };
    }
    macro_rules! exists {
        ($expr:expr) => {
            $expr.try_exists().is_ok_and(|b| b == true)
        };
    }
    pub(crate) use {exists, lossy_to_string};
}

#[derive(Default)]
struct MetricScores {
    frames: HashMap<usize, FrameMetrics>,
    stats: FrameMetrics,
    file: PathBuf,
}

/// Use metrics to score the quality of videos compared to a source.
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Source video path. Can be relative to this script or a full path.
    #[arg(long)]
    source: PathBuf,

    /// Encoded video path. Can be relative to this script or a full path. Multiple paths can be passed to compare multiple encoded videos.
    #[arg(long, num_args(1..), required=true)]
    encoded: Vec<PathBuf>,

    /// Scores JSON path. Can be relative to this script or a full path. SSIMULACRApy will save the scores to this path in JSON format.
    #[arg(short, long, default_value = None)]
    scores: Option<PathBuf>,

    /// Graph PNG path. Can be relative to this script or a full path. SSIMULACRApy will save the scores as a graph to this path in PNG format. If the metric is Butteraugli, -2Norm, -3Norm, and -INFNorm will be appended to the filename.
    #[arg(short, long, default_value = None)]
    graph: Option<PathBuf>,

    /// Graph PNG width
    #[arg(long, default_value_t = 1500)]
    graph_width: u32,

    /// Graph PNG height
    #[arg(long, default_value_t = 1000)]
    graph_height: u32,

    /// Don't ask for confirmation, answer yes.
    #[arg(short, long, default_value_t = false)]
    yes: bool,

    /// Video importer for the source and encoded videos.
    #[arg(short, long, value_parser(["lsmash","dgdecnv","ffms2","bestsource"]), default_value = "bestsource")]
    importer: String,

    /// Metric to use.
    #[arg(short, long, value_parser(["ssimu2_vszip","ssimu2_vship","butter_vship","psnr_turbo","ssim_turbo","msssim_turbo","ssimu2_turbo"]), default_value = "ssimu2_vszip")]
    metric: String,

    /// Number of threads to use.
    #[arg(short, long, default_value_t = threads().unwrap_or(NonZero::new(1).unwrap()))]
    threads: NonZeroUsize,

    /// Video width to downscale to.
    #[arg(long, default_value = None)]
    width: Option<NonZeroUsize>,

    /// Video height to downscale to.
    #[arg(long, default_value = None)]
    height: Option<NonZeroUsize>,

    /// Frames calculated every nth frame. For example, setting this to 5 will calculate every 5th frame.
    #[arg(short, long, default_value_t = NonZero::new(1).unwrap())]
    every: NonZeroUsize,

    /// Start frame.
    #[arg(long, default_value_t = 0)]
    start: usize,

    /// End frame.
    #[arg(long, default_value = None)]
    end: Option<usize>,

    /// Enable verbose output.
    #[arg(short, long, default_value_t = false)]
    verbose: bool,
}

fn main() -> Result<(), String> {
    let args = Args::parse();
    let metric = method_to_metric(&args.metric);
    let mut scores = Scores::new(args.scores.as_ref());
    if args.verbose {
        println!("Source: {}", args.source.display());
        println!("Encoded: {}", args.encoded.len());
        for path in args.encoded.iter() {
            println!("{}", path.display());
        }
        if args.scores.is_some() {
            println!("Score file: {}", args.scores.as_ref().unwrap().display());
        }
        println!("Importer: {}", args.importer);
    }
    let mut encoded_clip_hash: HashMap<String, String> = HashMap::new();
    let mut skipped_file: Vec<usize> = Vec::new();
    // TODO: make task_id a HashMap containing progress bars
    let multi_prog = MultiProgress::new();
    multi_prog.set_draw_target(ProgressDrawTarget::stderr());
    let mut task_id: HashMap<String, (ProgressBar, usize)> = HashMap::new();
    for (idx, file) in args.encoded.iter().enumerate() {
        let scores_path = args.scores.as_ref();
        let file_name = lossy_to_string!(file.file_name().unwrap());
        encoded_clip_hash.insert(file_name.clone(), b2sum(&file)?);
        let clip_hash = &encoded_clip_hash[&file_name];

        if scores_path.is_none() || !exists!(scores_path.unwrap()) {
            task_id.insert(clip_hash.clone(), (multi_prog.add(mk_bar()), idx + 1));
            continue;
        }

        if args.scores.is_some() {
            scores.load().unwrap();
        }
        if !scores.exists(&args.source, clip_hash, &metric) {
            task_id.insert(clip_hash.clone(), (multi_prog.add(mk_bar()), idx + 1));
            continue;
        }

        skipped_file.push(idx);
        println!("Skipping {file_name}...");

        let frames = scores.get_frames(&args.source, clip_hash);
        let final_scores = scores.get_final_scores(&args.source, clip_hash);
        let options = final_scores.get(&metric).unwrap();
        let stats = MetricStats::try_from((&options, &args.metric)).ok();
        stats.as_ref().inspect(|d| println!("{d:#?}"));
        let avg = stats.as_ref().map(|d| d.average);
        // add new task for skipped file

        task_id.insert(clip_hash.clone(), (multi_prog.add(mk_bar()), idx + 1));
        let bar = &task_id[clip_hash].0;
        bar.finish_and_clear();

        bar.set_position(frames as u64);
        bar.set_length(frames as u64);
        bar.reset_elapsed();
        if avg.is_some() {
            bar.set_message(format!(" AVG: Score: {:.5}", avg.unwrap()));
        }
    }

    if skipped_file.len() == args.encoded.len() {
        println!("No files to process.");
        exit(0);
    }

    let mut files_to_process = args.encoded.clone();

    if skipped_file.len() > 0 {
        for i in skipped_file {
            files_to_process.remove(i);
        }
    }

    scores.add_video(&files_to_process, &encoded_clip_hash, &args.source, &args);

    let mut metric_scores: HashMap<String, MetricScores> = HashMap::new();

    for file in files_to_process.iter() {
        let file_name = file.file_name().unwrap().to_string_lossy().to_string();
        let hash = &encoded_clip_hash[&file_name];
        let bar = &task_id[hash].0;
        init_bar(bar, 0, task_id[hash].1);
        metric_scores.insert(hash.clone(), MetricScores::default());

        let metrics = Metrics::new(&args.source, file, &args);
        let (sender, receiver) = channel();
        metrics.run(&sender, &bar)?;
        drop(sender);
        for output in receiver.iter() {
            if output.message.is_some() {
                let msg = output.message.as_ref().unwrap();
                bar.println(&msg.msg);
                if msg.exit {
                    exit(1);
                }
                continue;
            }
            let (frame, num_frames, metrics) = (output.frame, output.total_frames, output.score);
            let score = score_from_metrics(&metrics, &args.metric);
            let m_scores = metric_scores.get_mut(hash).unwrap();
            m_scores.frames.insert(frame, metrics.clone());

            if num_frames.is_some() {
                bar.set_length(num_frames.clone().unwrap() as u64);
            }

            if args.verbose {
                let formatted = if metric == "butteraugli" {
                    type Hamburger = (f64, f64, f64);
                    let (norm2, norm3, infnorm) = Hamburger::try_from(score).unwrap();
                    format!("2Norm: {norm2} 3Norm: {norm3} INFNorm: {infnorm}")
                } else {
                    f64::try_from(score).unwrap().to_string()
                };
                println!(
                    "Frame {frame}/{}: {formatted}",
                    num_frames.map(|n| n.to_string()).unwrap_or("?".to_string())
                )
            }

            scores.add_frame(&args.source, hash, frame, metrics);

            let current_scores = scores.get_scores(&args.source, &hash);
            let calculated = calculate_scores(current_scores);
            let options = calculated.get(&metric).unwrap();
            let stats = MetricStats::try_from((&options, &args.metric)).ok();
            let avg = stats.as_ref().map(|d| d.average);

            if avg.is_some() {
                bar.set_message(format!(" AVG: Score: {:.5}", avg.unwrap()));
            }
            bar.inc(1);
        }
        bar.finish();
        let item = metric_scores.get_mut(hash).unwrap();
        let calculated = calculate_scores(item.frames.values().collect());
        item.stats = calculated;
        item.file = file.clone();

        scores.finalize_video(&args.source, hash);

        if args.scores.is_some() {
            let result = scores.save();
            if result.is_err() {
                eprintln!("Failed to save scores to file! {}", result.err().unwrap());
            }
        }
    }

    fn print_scores(stats: &MetricStats, name: &str) {
        println!(
            "{name}\n  Average: {}\n  Standard deviation: {}\n  Median: {}\n  Percentile 5: {}\n  Percentile 95: {}",
            stats.average,
            stats.standard_deviation,
            stats.median,
            stats.percentile_5,
            stats.percentile_95
        );
    }

    for (_, scores) in metric_scores.iter() {
        let file = &scores.file;
        let file_name = file.file_name().unwrap().to_string_lossy().to_string();
        println!("{}: {file_name}", args.metric);
        let current_scores = scores.frames.values().collect();
        let calculated = calculate_scores(current_scores);
        let metrics = ["ssimulacra2", "butteraugli", "psnr", "ssim", "msssim"];
        for m in metrics {
            let item = calculated.get(&m.to_string());
            if !item.is_some() {
                continue;
            }

            if m == "butteraugli" {
                let butter = &calculated.butteraugli.as_ref().unwrap().butter_vship;
                print_scores(&MetricStats::try_from(&butter.norm2).unwrap(), "2Norm");
                print_scores(&MetricStats::try_from(&butter.norm3).unwrap(), "3Norm");
                print_scores(&MetricStats::try_from(&butter.infnorm).unwrap(), "INFNorm");
                continue;
            }

            let options = item.as_ref().unwrap();
            let stats = MetricStats::try_from((options, &args.metric)).unwrap();
            print_scores(&stats, m);
        }
    }

    if args.graph.is_some() {
        let graph = args.graph.as_ref().unwrap();
        for file in &args.encoded {
            let file_name = lossy_to_string!(file.file_name().unwrap());
            let source_name = lossy_to_string!(args.source.file_name().unwrap());
            let hash = &encoded_clip_hash[&file_name];
            let source = &scores.scores.source.get(&source_name);
            if source.is_none() {
                continue;
            }
            let source = source.unwrap();
            let encoded = &source.encoded.get(hash);
            if encoded.is_none() {
                continue;
            }
            let encoded = encoded.unwrap();
            let frames = encoded.scores.frame.len() * encoded.every.get();
            let scores = encoded
                .scores
                .frame
                .iter()
                .map(|d| (d.0, score_from_metrics(&d.1, &args.metric)));
            let metric = match metric.as_str() {
                "butteraugli" => "Butteraugli".to_string(),
                _ => metric.to_uppercase(),
            };
            if metric == "Butteraugli" {
                type Hamburger = (f64, f64, f64);
                let butter = scores.map(|d| (d.0, Hamburger::try_from(d.1).unwrap()));
                let (norm2, norm3, infnorm) = (
                    butter.clone().map(|d| (*d.0, d.1.0)),
                    butter.clone().map(|d| (*d.0, d.1.1)),
                    butter.map(|d| (*d.0, d.1.2)),
                );
                fn append(path: &PathBuf, hi: &str) -> PathBuf {
                    let mut path = path.clone();
                    path.set_file_name(format!(
                        "{}-{hi}.png",
                        path.file_stem().unwrap().to_string_lossy()
                    ));
                    path
                }
                for name in ["2Norm", "3Norm", "INFNorm"] {
                    let hi: BTreeMap<usize, f64> = match name {
                        "2Norm" => norm2.clone().collect(),
                        "3Norm" => norm3.clone().collect(),
                        "INFNorm" => infnorm.clone().collect(),
                        _ => unreachable!(),
                    };
                    let max = hi
                        .iter()
                        .map(|d| *d.1 as f32)
                        .fold(f32::INFINITY, |a, b| a.min(b));
                    mk_graph(
                        &append(graph, name),
                        args.yes,
                        frames,
                        hi,
                        args.graph_width,
                        args.graph_height,
                        0f32..max + 2.,
                        name,
                        &metric,
                    );
                }
            } else {
                let scores = scores.map(|d| (*d.0, f64::try_from(d.1).unwrap()));
                mk_graph(
                    graph,
                    args.yes,
                    frames,
                    scores.collect(),
                    args.graph_width,
                    args.graph_height,
                    0f32..100f32,
                    "Score",
                    &metric,
                );
            }
        }
    }

    Ok(())
}
