use human_bytes::human_bytes as size;
use indicatif::{FormattedDuration, ProgressBar, ProgressDrawTarget, ProgressState, ProgressStyle};
use std::time::Duration;

pub fn mk_style_hash() -> ProgressStyle {
    ProgressStyle::with_template("{spinner:.green.bold} {prefix:!.bold} {bar:40.green/black}{_percent:.magenta} {progress:.green} {bytes_per_sec} {elapsed_precise:.yellow} ETA: {_eta:.cyan}")
        .unwrap()
        .with_key("_percent", |state: &ProgressState, w: &mut dyn std::fmt::Write| {
            if state.len().is_none_or(|n| n==0) {
                write!(w, "").unwrap();
            } else {
                write!(w, " {}%", state.fraction() * 100.).unwrap();
            }
        })
        .with_key("_eta", |state: &ProgressState, w: &mut dyn std::fmt::Write| {
            if state.len().is_none_or(|n| n==0) {
                write!(w, "-:--:--").unwrap();
            } else {
                write!(w, "{}", FormattedDuration(state.eta())).unwrap();
            }
        })
        .with_key("progress", |state: &ProgressState, w: &mut dyn std::fmt::Write| {
            write!(w, "{}/{}", size(state.pos() as f64), size(state.len().unwrap() as f64)).unwrap();
        })
        .progress_chars("━━")
}

pub fn mk_style() -> ProgressStyle {
    ProgressStyle::with_template(
        "{spinner:.green.bold} {prefix:.bold} {bar:40.green/black}{_percent:.magenta} {progress:.green} {fps} {elapsed_precise:.yellow} ETA: {_eta:.cyan}{msg}",
    )
    .unwrap()
    .with_key("_percent", |state: &ProgressState, w: &mut dyn std::fmt::Write| {
        if state.len().is_none_or(|n| n==0) {
            write!(w, "").unwrap();
        } else {
            write!(w, " {:.2}%", state.fraction() * 100.).unwrap();
        }
    })
    .with_key("_eta", |state: &ProgressState, w: &mut dyn std::fmt::Write| {
        if state.len().is_none_or(|n| n==0) {
            write!(w, "-:--:--").unwrap();
        } else {
            write!(w, "{}", FormattedDuration(state.eta())).unwrap();
        }
    })
    .with_key("progress", |state: &ProgressState, w: &mut dyn std::fmt::Write| {
        write!(w, "{}/{}", state.pos(), state.len().map(|n|n.to_string()).unwrap_or("?".into())).unwrap();
    })
    .with_key(
        "fps",
        |state: &ProgressState, w: &mut dyn std::fmt::Write| {
            if state.pos() == 0 || state.elapsed().as_secs_f32() < f32::EPSILON {
                write!(w, "0 fps").unwrap();
            } else {
                let fps = state.pos() as f32 / state.elapsed().as_secs_f32();
                if fps < 1.0 {
                    write!(w, "{:.2} s/fr", 1.0 / fps).unwrap();
                } else if fps <= 0.0 {
                    write!(w, "0 fps").unwrap();
                } else {
                    write!(w, "{:.2} fps", fps).unwrap();
                }
            }
        },
    )
    .progress_chars("━━")
}

pub fn mk_bar() -> ProgressBar {
    let pb = ProgressBar::new_spinner().with_style(mk_style());
    pb.set_draw_target(ProgressDrawTarget::stderr());
    pb
}

pub fn init_bar(pb: &ProgressBar, pos: usize, idx: usize) {
    pb.enable_steady_tick(Duration::from_millis(100));
    pb.reset();
    pb.reset_eta();
    pb.reset_elapsed();
    pb.set_position(pos as u64);
    pb.set_prefix(format!("Calculating File {idx}:"));
}
