use crate::progress::mk_style_hash;
use crate::scores::{
    ButteraugliData, ButteraugliMetrics, FrameMetrics, MSSSIMData, MSSSIMMetrics, MetricStats,
    PSNRData, PSNRMetrics, SSIMData, SSIMMetrics, SSIMULACRA2Data, SSIMULACRA2Metrics,
};
use blake2::{Blake2b, Digest, digest::consts::U16};
use image::ColorType;
use indicatif::{ProgressBar, ProgressDrawTarget};
use plotters::prelude::*;
use std::collections::BTreeMap;
use std::io::{BufRead, stdin};
use std::ops::Range;
use std::process::exit;
use std::time::Duration;
use std::{fs::File, io::copy, path::PathBuf};
use vapoursynth4_rs::api::Api;
use vapoursynth4_rs::core::Core;

macro_rules! unwrapref {
    ($expr:expr) => {
        $expr.as_ref().unwrap()
    };
}

#[derive(Default)]
struct ButteraugliScores {
    norm2: Vec<f64>,
    norm3: Vec<f64>,
    infnorm: Vec<f64>,
}

pub enum Score {
    Normal(f64),
    Butter((f64, f64, f64)),
}
impl TryFrom<Score> for f64 {
    type Error = String;
    fn try_from(value: Score) -> Result<Self, Self::Error> {
        if let Score::Normal(d) = value {
            Ok(d)
        } else {
            Err("Can't convert Butteraugli score to f64!".to_string())
        }
    }
}
impl TryFrom<Score> for (f64, f64, f64) {
    type Error = String;
    fn try_from(value: Score) -> Result<Self, Self::Error> {
        if let Score::Butter(d) = value {
            Ok(d)
        } else {
            Err("Can't convert score to (f64, f64, f64)!".to_string())
        }
    }
}

#[derive(Default)]
struct MetricScores {
    ssimu2_vszip: Vec<f64>,
    ssimu2_vship: Vec<f64>,
    ssimu2_turbo: Vec<f64>,
    butter_vship: ButteraugliScores,
    psnr_turbo: Vec<f64>,
    ssim_turbo: Vec<f64>,
    msssim_turbo: Vec<f64>,
}

/// Generate a BLAKE2b hash of a file.
pub fn b2sum(file: &PathBuf) -> Result<String, String> {
    type Blake2bU16 = Blake2b<U16>;
    let open = File::open(file).map_err(|e| format!("Failed to read {}: {e}", file.display()))?;
    let size = open.metadata().map_err(|e| e.to_string())?.len();
    let pb = ProgressBar::new(size).with_style(mk_style_hash());
    pb.set_draw_target(ProgressDrawTarget::stderr());
    pb.enable_steady_tick(Duration::from_millis(100));
    pb.set_position(0);
    pb.set_prefix(format!(
        "Calculating File Hash for {:?}:",
        file.file_name().unwrap()
    ));
    let mut buffer = Vec::new();
    copy(&mut pb.wrap_read(open), &mut buffer)
        .map_err(|e| format!("Failed to read {}: {e}", file.display()))?;
    let digest = format!("{:x}", Blake2bU16::digest(buffer));
    pb.finish_and_clear();
    Ok(digest)
}

fn generate_stats(data: Vec<f64>) -> Option<MetricStats> {
    let mut filtered: Vec<f64> = data.iter().filter(|n| n > &&0f64).map(|n| *n).collect();
    filtered.sort_by(|a, b| a.partial_cmp(b).unwrap());
    if filtered.len() <= 0 {
        return None;
    }
    let sum: f64 = filtered.iter().sum();
    let square_sum: f64 = filtered.iter().map(|n| n.powi(2)).sum();
    let len: f64 = filtered.len() as f64;
    let variance: f64 = (square_sum / len - (sum / len).powi(2)).max(0.0);
    let _hi = (-len / 20f64).floor();
    Some(MetricStats {
        average: sum / len,
        standard_deviation: variance.sqrt(),
        median: filtered[(len / 2f64).floor() as usize],
        percentile_5: if len >= 20f64 {
            filtered[(len / 20f64).floor() as usize]
        } else {
            filtered[0]
        },
        percentile_95: if len >= 20f64 {
            *filtered
                .iter()
                .rev()
                .nth((-len / 20f64 + 1.).floor().abs() as usize)
                .unwrap()
        } else {
            *filtered.last().unwrap()
        },
    })
}

/// Calculate scores for metric structure
pub fn calculate_scores(score_list: Vec<&FrameMetrics>) -> FrameMetrics {
    let mut result = FrameMetrics::default();
    let mut scores = MetricScores::default();
    for frame in score_list {
        if frame.ssimulacra2.is_some() {
            let ssimu2 = unwrapref!(frame.ssimulacra2);
            if ssimu2.ssimu2_vszip.is_some() {
                scores
                    .ssimu2_vszip
                    .push(f64::from(&unwrapref!(ssimu2.ssimu2_vszip).ssimulacra2));
            }
            if ssimu2.ssimu2_vship.is_some() {
                scores
                    .ssimu2_vship
                    .push(f64::from(&unwrapref!(ssimu2.ssimu2_vship).ssimulacra2));
            }
            if ssimu2.ssimu2_turbo.is_some() {
                scores
                    .ssim_turbo
                    .push(f64::from(&unwrapref!(ssimu2.ssimu2_turbo).ssimulacra2));
            }
        }
        if frame.butteraugli.is_some() {
            let butter = &unwrapref!(frame.butteraugli).butter_vship;
            scores.butter_vship.norm2.push(f64::from(&butter.norm2));
            scores.butter_vship.norm3.push(f64::from(&butter.norm3));
            scores.butter_vship.infnorm.push(f64::from(&butter.infnorm));
        }
        if frame.psnr.is_some() {
            let psnr = &unwrapref!(frame.psnr).psnr_turbo.psnr;
            scores.psnr_turbo.push(f64::from(psnr));
        }
        if frame.ssim.is_some() {
            let ssim = &unwrapref!(frame.ssim).ssim_turbo.ssim;
            scores.ssim_turbo.push(f64::from(ssim));
        }
        if frame.msssim.is_some() {
            let msssim = &unwrapref!(frame.msssim).msssim_turbo.msssim;
            scores.msssim_turbo.push(f64::from(msssim));
        }
    }
    if !scores.ssimu2_vszip.is_empty()
        || !scores.ssimu2_vship.is_empty()
        || !scores.ssimu2_turbo.is_empty()
    {
        result.ssimulacra2 = Some(SSIMULACRA2Metrics {
            ssimu2_vszip: SSIMULACRA2Data::try_from(generate_stats(scores.ssimu2_vszip)).ok(),
            ssimu2_vship: SSIMULACRA2Data::try_from(generate_stats(scores.ssimu2_vship)).ok(),
            ssimu2_turbo: SSIMULACRA2Data::try_from(generate_stats(scores.ssimu2_turbo)).ok(),
        });
    }
    // since one value is filled out, surely the others must be
    if !scores.butter_vship.infnorm.is_empty() {
        let norm2 = generate_stats(scores.butter_vship.norm2).unwrap();
        let norm3 = generate_stats(scores.butter_vship.norm3).unwrap();
        let infnorm = generate_stats(scores.butter_vship.infnorm).unwrap();
        result.butteraugli = Some(ButteraugliMetrics {
            butter_vship: ButteraugliData::from((norm2, norm3, infnorm)),
        });
    }
    if !scores.psnr_turbo.is_empty() {
        result.psnr = Some(PSNRMetrics {
            psnr_turbo: PSNRData::from(generate_stats(scores.psnr_turbo).unwrap()),
        });
    }
    if !scores.ssim_turbo.is_empty() {
        result.ssim = Some(SSIMMetrics {
            ssim_turbo: SSIMData::from(generate_stats(scores.ssim_turbo).unwrap()),
        });
    }
    if !scores.msssim_turbo.is_empty() {
        result.msssim = Some(MSSSIMMetrics {
            msssim_turbo: MSSSIMData::from(generate_stats(scores.msssim_turbo).unwrap()),
        });
    }
    result
}

pub fn method_to_metric(method: &String) -> String {
    match method.as_str() {
        "ssimu2_vszip" | "ssimu2_vship" | "ssimu2_turbo" => "ssimulacra2",
        "butter_vship" => "butteraugli",
        "psnr_turbo" => "psnr",
        "ssim_turbo" => "ssim",
        "msssim_turbo" => "msssim",
        _ => panic!("Unknown metric!"),
    }
    .to_string()
}

pub fn score_from_metrics(metrics: &FrameMetrics, method: &String) -> Score {
    let ssimu2 = metrics.ssimulacra2.as_ref();
    let butter = metrics.butteraugli.as_ref();
    let psnr = metrics.psnr.as_ref();
    let ssim = metrics.ssim.as_ref();
    let msssim = metrics.msssim.as_ref();
    macro_rules! ssimu2_method {
        ($expr:expr, $method:ident) => {
            $expr.map(|m| m.$method.as_ref()).unwrap_or(None)
        };
    }
    let (vszip, ssimu2_vship, ssimu2_turbo) = (
        ssimu2_method!(ssimu2, ssimu2_vszip),
        ssimu2_method!(ssimu2, ssimu2_vship),
        ssimu2_method!(ssimu2, ssimu2_turbo),
    );
    match method.as_str() {
        "ssimu2_vszip" => Score::Normal(f64::from(&vszip.unwrap().ssimulacra2)),
        "ssimu2_vship" => Score::Normal(f64::from(&ssimu2_vship.unwrap().ssimulacra2)),
        "ssimu2_turbo" => Score::Normal(f64::from(&ssimu2_turbo.unwrap().ssimulacra2)),
        "butter_vship" => Score::Butter((
            f64::from(&butter.unwrap().butter_vship.norm2),
            f64::from(&butter.unwrap().butter_vship.norm3),
            f64::from(&butter.unwrap().butter_vship.infnorm),
        )),
        "psnr_turbo" => Score::Normal(f64::from(&psnr.unwrap().psnr_turbo.psnr)),
        "ssim_turbo" => Score::Normal(f64::from(&ssim.unwrap().ssim_turbo.ssim)),
        "msssim_turbo" => Score::Normal(f64::from(&msssim.unwrap().msssim_turbo.msssim)),
        _ => unreachable!(),
    }
}

pub fn available_importers() -> Vec<String> {
    let api = Api::default();
    let core = Core::builder().api(api).disable_library_unloading().build();
    let mut list: Vec<String> = Vec::new();
    for plugin in core.plugins() {
        match plugin.id().to_str().unwrap() {
            "com.vapoursynth.bestsource" => list.push("bestsource".to_string()),
            "systems.innocent.lsmas" => list.push("lsmash".to_string()),
            "com.vapoursynth.ffms2" => list.push("ffms2".to_string()),
            "com.vapoursynth.dgdecodenv" => list.push("dgdecnv".to_string()),
            _ => continue,
        }
    }
    list
}

fn input(msg: &String) {
    println!("{msg}");
    let stdin = stdin();
    let mut iterator = stdin.lock().lines();
    let response = iterator.next().unwrap().unwrap().to_lowercase();
    if !["y", "n", "", "yes", "no"].contains(&response.as_str()) {
        input(&"Input not recognized. (Y/n): ".to_string());
    } else if ["n", "no"].contains(&response.as_str()) {
        eprintln!("Input is \"{response}\", exiting!");
        exit(1);
    }
}

pub fn mk_graph(
    out: &PathBuf,
    yes: bool,
    frames: usize,
    scores: BTreeMap<usize, f64>,
    width: u32,
    height: u32,
    range: Range<f32>,
    desc: &str,
    metric: &String,
) {
    if out.try_exists().is_ok_and(|b| b == true) && !yes {
        input(&format!(
            "File {} already exists! Overwrite? (Y/n): ",
            out.display()
        ));
    }
    let mut image_buffer = vec![0; (width * height * 3) as usize].into_boxed_slice();
    {
        let root =
            BitMapBackend::with_buffer(&mut image_buffer, (width, height)).into_drawing_area();
        root.fill(&BLACK).unwrap();

        let mut chart = ChartBuilder::on(&root)
            .set_label_area_size(LabelAreaPosition::Left, 60)
            .set_label_area_size(LabelAreaPosition::Bottom, 60)
            .caption(metric, ("sans-serif", 50.))
            .build_cartesian_2d(0..frames, range)
            .unwrap();
        chart
            .configure_mesh()
            .disable_x_mesh()
            .bold_line_style(WHITE.mix(0.3))
            .y_desc(desc)
            .y_label_style(("sans-serif", 16, &WHITE))
            .x_desc("Frame")
            .x_label_style(("sans-serif", 16, &WHITE))
            .axis_desc_style(("sans-serif", 16, &WHITE))
            .draw()
            .unwrap();
        chart
            .draw_series(
                AreaSeries::new(
                    scores.iter().map(|(i, v)| (*i, *v as f32)),
                    0.,
                    CYAN.mix(0.5),
                )
                .border_style(CYAN.filled()),
            )
            .unwrap();
        root.present().expect("Unable to generate image!");
    }

    image::save_buffer(&out, &image_buffer, width, height, ColorType::Rgb8)
        .expect("Unable to save graph image!");
}
