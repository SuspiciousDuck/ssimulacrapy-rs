use crate::Args;
use crate::method_to_metric;
use crate::scores::ButteraugliData;
use crate::scores::ButteraugliMetrics;
use crate::scores::{
    FrameMetrics, MSSSIMData, MSSSIMMetrics, PSNRData, PSNRMetrics, SSIMData, SSIMMetrics,
    SSIMULACRA2Data, SSIMULACRA2Metrics, ScoreData,
};
use crate::utils::available_importers;
use indicatif::ProgressBar;
use serde_json::Value;
use std::ffi::{CStr, CString};
use std::fs::File;
use std::io::Read;
use std::sync::{Arc, Mutex};
use std::thread;
use std::{
    collections::HashMap,
    io::{BufRead, BufReader},
    num::NonZeroUsize,
    path::{PathBuf, absolute as abs},
    process::{Command, Stdio},
    sync::mpsc::Sender,
};
use vapoursynth4_rs::api::Api;
use vapoursynth4_rs::core::Core;
use vapoursynth4_rs::ffi::VSMapAppendMode::Replace;
use vapoursynth4_rs::ffi::{VSSCRIPT_API_VERSION, getVSScriptAPI};
use vapoursynth4_rs::frame::Frame;
use vapoursynth4_rs::map::{KeyStr as KStr, Map, Value as MValue};
use vapoursynth4_rs::node::Node;
use vapoursynth4_rs::node::VideoNode;
use vapoursynth4_rs::utils::ToCString;

#[derive(Debug, Default)]
pub struct MetricResult {
    pub frame: usize,
    pub total_frames: Option<usize>,
    pub score: FrameMetrics,
    pub message: Option<Message>,
}

#[derive(Debug, Default)]
pub struct Message {
    pub level: String,
    pub msg: String,
    pub exit: bool,
}

macro_rules! maperr {
    ($expr:expr) => {{ $expr.map_err(|e| e.to_string()) }};
}

macro_rules! cstr {
    ($expr:expr) => {
        $expr.into_cstring_lossy().as_c_str()
    };
}

pub struct Metrics {
    source: PathBuf,
    encoded: PathBuf,
    width: Option<NonZeroUsize>,
    height: Option<NonZeroUsize>,
    threads: NonZeroUsize,
    metric: String,
    method: String,
    importer: String,
    every: NonZeroUsize,
    start: usize,
    end: Option<usize>,
}
impl Metrics {
    pub fn new(source: &PathBuf, encoded: &PathBuf, args: &Args) -> Self {
        Self {
            source: source.clone(),
            encoded: encoded.clone(),
            width: args.width,
            height: args.height,
            threads: args.threads,
            metric: method_to_metric(&args.metric),
            method: args.metric.clone(),
            importer: args.importer.clone(),
            every: args.every.clone(),
            start: args.start.clone(),
            end: args.end.clone(),
        }
    }
    fn bestsource(path: &PathBuf, core: &Core) -> Result<VideoNode, String> {
        let bestsource = core
            .get_plugin_by_id(cstr!("com.vapoursynth.bestsource"))
            .ok_or("Failed to get bestsource namespace! Is the plugin installed?".to_string())?;
        let abspath = maperr!(abs(path))?;
        let mut root = PathBuf::from(abspath.components().next().unwrap().as_os_str());
        if !root.ends_with("/") {
            root.push("/")
        };
        let mut args = Map::default();
        maperr!(args.set(
            KStr::from_cstr(cstr!("source")),
            MValue::Utf8(abspath.to_str().unwrap()),
            Replace,
        ))?;
        maperr!(args.set(
            KStr::from_cstr(cstr!("cachepath")),
            MValue::Utf8(root.to_str().unwrap()),
            Replace,
        ))?;
        let func = bestsource.invoke(cstr!("VideoSource"), args);
        func.get_error()
            .map(|e| Err(e.to_string_lossy().to_string()))
            .or(Some(Ok(())))
            .unwrap()?;
        Ok(maperr!(
            func.get_video_node(KStr::from_cstr(cstr!("clip")), 0)
        )?)
    }
    fn lsmash(path: &PathBuf, core: &Core) -> Result<VideoNode, String> {
        let lsmashsource = core
            .get_plugin_by_id(cstr!("systems.innocent.lsmas"))
            .ok_or("Failed to get lsmashsource namespace! Is the plugin installed?".to_string())?;
        let abspath = maperr!(abs(path))?;
        let mut args = Map::default();
        maperr!(args.set(
            KStr::from_cstr(cstr!("source")),
            MValue::Utf8(abspath.to_str().unwrap()),
            Replace,
        ))?;
        let func = lsmashsource.invoke(cstr!("LWLibavSource"), args);
        func.get_error()
            .map(|e| Err(e.to_string_lossy().to_string()))
            .or(Some(Ok(())))
            .unwrap()?;
        Ok(maperr!(
            func.get_video_node(KStr::from_cstr(cstr!("clip")), 0)
        )?)
    }
    fn ffms2(path: &PathBuf, core: &Core) -> Result<VideoNode, String> {
        let ffms2 = core
            .get_plugin_by_id(cstr!("com.vapoursynth.ffms2"))
            .ok_or("Failed to get ffms2 namespace! Is the plugin installed?".to_string())?;
        let abspath = maperr!(abs(path))?;
        let mut args = Map::default();
        maperr!(args.set(
            KStr::from_cstr(cstr!("source")),
            MValue::Utf8(abspath.to_str().unwrap()),
            Replace,
        ))?;
        let func = ffms2.invoke(cstr!("Source"), args);
        func.get_error()
            .map(|e| Err(e.to_string_lossy().to_string()))
            .or(Some(Ok(())))
            .unwrap()?;
        Ok(maperr!(
            func.get_video_node(KStr::from_cstr(cstr!("clip")), 0)
        )?)
    }
    fn trim(
        core: &Core,
        clip: VideoNode,
        start: usize,
        end: Option<usize>,
    ) -> Result<VideoNode, String> {
        let std = core
            .get_plugin_by_id(cstr!("com.vapoursynth.std"))
            .ok_or("Failed to get std namespace!".to_string())?;
        let mut args = Map::default();
        maperr!(args.set(
            KStr::from_cstr(cstr!("clip")),
            MValue::VideoNode(clip),
            Replace,
        ))?;
        maperr!(args.set(
            KStr::from_cstr(cstr!("first")),
            MValue::Int(start as i64),
            Replace,
        ))?;
        if end.is_some() {
            maperr!(args.set(
                KStr::from_cstr(cstr!("last")),
                MValue::Int(end.unwrap() as i64),
                Replace,
            ))?;
        }
        let func = std.invoke(cstr!("Trim"), args);
        func.get_error()
            .map(|e| Err(e.to_string_lossy().to_string()))
            .or(Some(Ok(())))
            .unwrap()?;
        Ok(maperr!(
            func.get_video_node(KStr::from_cstr(cstr!("clip")), 0)
        )?)
    }
    fn select_every(
        core: &Core,
        clip: VideoNode,
        every: &NonZeroUsize,
    ) -> Result<VideoNode, String> {
        let std = core
            .get_plugin_by_id(cstr!("com.vapoursynth.std"))
            .ok_or("Failed to get std namespace!".to_string())?;
        let mut args = Map::default();
        maperr!(args.set(
            KStr::from_cstr(cstr!("clip")),
            MValue::VideoNode(clip),
            Replace,
        ))?;
        maperr!(args.set(
            KStr::from_cstr(cstr!("cycle")),
            MValue::Int(every.get() as i64),
            Replace,
        ))?;
        maperr!(args.set(KStr::from_cstr(cstr!("offsets")), MValue::Int(0), Replace,))?;
        let func = std.invoke(cstr!("SelectEvery"), args);
        func.get_error()
            .map(|e| Err(e.to_string_lossy().to_string()))
            .or(Some(Ok(())))
            .unwrap()?;
        Ok(maperr!(
            func.get_video_node(KStr::from_cstr(cstr!("clip")), 0)
        )?)
    }
    fn bicubic(
        core: &Core,
        clip: VideoNode,
        width: Option<NonZeroUsize>,
        height: Option<NonZeroUsize>,
    ) -> Result<VideoNode, String> {
        if width.is_none() && height.is_none() {
            return Ok(clip);
        }
        let std = core
            .get_plugin_by_id(cstr!("com.vapoursynth.resize"))
            .ok_or("Failed to get resize namespace!".to_string())?;
        let mut args = Map::default();
        maperr!(args.set(
            KStr::from_cstr(cstr!("clip")),
            MValue::VideoNode(clip),
            Replace,
        ))?;
        if width.is_some() {
            maperr!(args.set(
                KStr::from_cstr(cstr!("width")),
                MValue::Int(width.unwrap().get() as i64),
                Replace,
            ))?;
        }
        if height.is_some() {
            maperr!(args.set(
                KStr::from_cstr(cstr!("height")),
                MValue::Int(height.unwrap().get() as i64),
                Replace,
            ))?;
        }
        let func = std.invoke(cstr!("Bicubic"), args);
        func.get_error()
            .map(|e| Err(e.to_string_lossy().to_string()))
            .or(Some(Ok(())))
            .unwrap()?;
        Ok(maperr!(
            func.get_video_node(KStr::from_cstr(cstr!("clip")), 0)
        )?)
    }
    fn vszip(
        core: &Core,
        reference: &VideoNode,
        distorted: &VideoNode,
    ) -> Result<VideoNode, String> {
        let std = core
            .get_plugin_by_id(cstr!("com.julek.vszip"))
            .ok_or("Failed to get vszip namespace! Is the plugin installed?".to_string())?;
        let mut args = Map::default();
        maperr!(args.set(
            KStr::from_cstr(cstr!("reference")),
            MValue::VideoNode(reference.clone()),
            Replace,
        ))?;
        maperr!(args.set(
            KStr::from_cstr(cstr!("distorted")),
            MValue::VideoNode(distorted.clone()),
            Replace,
        ))?;
        maperr!(args.set(KStr::from_cstr(cstr!("mode")), MValue::Int(0), Replace))?;
        let func = std.invoke(cstr!("Metrics"), args);
        func.get_error()
            .map(|e| Err(e.to_string_lossy().to_string()))
            .or(Some(Ok(())))
            .unwrap()?;
        Ok(maperr!(
            func.get_video_node(KStr::from_cstr(cstr!("clip")), 0)
        )?)
    }
    fn ssimu2_vship(
        core: &Core,
        reference: &VideoNode,
        distorted: &VideoNode,
    ) -> Result<VideoNode, String> {
        let std = core
            .get_plugin_by_id(cstr!("com.lumen.vship"))
            .ok_or("Failed to get vszip namespace! Is the plugin installed?".to_string())?;
        let mut args = Map::default();
        maperr!(args.set(
            KStr::from_cstr(cstr!("reference")),
            MValue::VideoNode(reference.clone()),
            Replace,
        ))?;
        maperr!(args.set(
            KStr::from_cstr(cstr!("distorted")),
            MValue::VideoNode(distorted.clone()),
            Replace,
        ))?;
        let func = std.invoke(cstr!("SSIMULACRA2"), args);
        func.get_error()
            .map(|e| Err(e.to_string_lossy().to_string()))
            .or(Some(Ok(())))
            .unwrap()?;
        Ok(maperr!(
            func.get_video_node(KStr::from_cstr(cstr!("clip")), 0)
        )?)
    }
    fn butter_vship(
        core: &Core,
        reference: &VideoNode,
        distorted: &VideoNode,
    ) -> Result<VideoNode, String> {
        let std = core
            .get_plugin_by_id(cstr!("com.lumen.vship"))
            .ok_or("Failed to get vszip namespace! Is the plugin installed?".to_string())?;
        let mut args = Map::default();
        maperr!(args.set(
            KStr::from_cstr(cstr!("reference")),
            MValue::VideoNode(reference.clone()),
            Replace,
        ))?;
        maperr!(args.set(
            KStr::from_cstr(cstr!("distorted")),
            MValue::VideoNode(distorted.clone()),
            Replace,
        ))?;
        let func = std.invoke(cstr!("BUTTERAUGLI"), args);
        func.get_error()
            .map(|e| Err(e.to_string_lossy().to_string()))
            .or(Some(Ok(())))
            .unwrap()?;
        Ok(maperr!(
            func.get_video_node(KStr::from_cstr(cstr!("clip")), 0)
        )?)
    }
    fn import_vpy(core: &Core, path: &PathBuf) -> Result<VideoNode, String> {
        let file_name = path.file_name().unwrap().to_string_lossy().to_string();
        let c_file_name = CString::new(file_name).unwrap();
        let mut file = File::open(path).map_err(|e| e.to_string())?;
        let mut buffer = String::new();
        file.read_to_string(&mut buffer)
            .map_err(|e| e.to_string())?;
        let c_buffer = CString::new(buffer).map_err(|e| e.to_string())?;
        let vssapi = unsafe { &*getVSScriptAPI(VSSCRIPT_API_VERSION) };
        let env = unsafe { (vssapi.createScript)(core.as_ptr()) };
        unsafe { (vssapi.evalSetWorkingDir)(env, 1) };
        let result =
            unsafe { (vssapi.evaluateBuffer)(env, c_buffer.as_ptr(), c_file_name.as_ptr()) };
        if result != 0 {
            let err_ptr = unsafe { (vssapi.getError)(env) };
            let err_cstr = unsafe { CStr::from_ptr(err_ptr) };
            return Err(err_cstr.to_string_lossy().to_string());
        }
        Ok(unsafe { VideoNode::from_ptr((vssapi.getOutputNode)(env, 0), core.api()) })
    }
    fn import_video(core: &Core, path: &PathBuf, importer: &String) -> Result<VideoNode, String> {
        let name = path.file_name().unwrap().to_string_lossy().to_string();
        if path.extension().unwrap() == "vpy" {
            return Self::import_vpy(&core, &path);
        }

        macro_rules! import_with {
            ($expr:expr) => {
                match $expr {
                    "bestsource" => Self::bestsource(path, core),
                    "lsmash" => Self::lsmash(path, core),
                    "ffms2" => Self::ffms2(path, core),
                    "dgdecnv" => todo!(),
                    _ => unreachable!(),
                }
            };
        }

        let try_import = import_with!(importer.as_str());
        if try_import.is_ok() {
            return try_import;
        }
        eprintln!("{importer} error: {}", try_import.err().unwrap());
        eprintln!("Failed to import {name} with {importer}, using fallback.");

        let importers = available_importers();

        for import in importers.iter().map(|s| s.as_str()) {
            if import == importer {
                continue;
            }

            println!("Attempting to use {import}...");
            let try_import = import_with!(import);
            if try_import.is_ok() {
                return try_import;
            }
            eprintln!("{import} error: {}", try_import.err().unwrap());
            eprintln!("Failed to import {name} with {import}!");
        }
        Err(format!("No available importers could import {name}!"))
    }
    // native because we call it ourselves, instead of wrapping the turbo_metrics binary
    fn calc_native(
        &self,
        sender: &Sender<MetricResult>,
        progress: &ProgressBar,
    ) -> Result<(), String> {
        let api = Api::default();
        let core = Core::builder()
            .api(api)
            .disable_library_unloading()
            .max_cache_size(1024)
            .thread_count(self.threads.get() as i32)
            .build();
        let notrim_source = Self::import_video(&core, &self.source, &self.importer)?;
        let notrim_encoded = Self::import_video(&core, &self.encoded, &self.importer)?;
        progress.reset_eta(); // so indexing doesnt mess up fps/eta
        let (source, encoded) = match (self.start.cmp(&0), self.end) {
            (std::cmp::Ordering::Greater, Some(e)) => (
                Self::trim(&core, notrim_source, self.start, Some(e - 1))?,
                Self::trim(&core, notrim_encoded, self.start, Some(e - 1))?,
            ),
            (std::cmp::Ordering::Greater, None) => (
                Self::trim(&core, notrim_source, self.start, None)?,
                Self::trim(&core, notrim_encoded, self.start, None)?,
            ),
            (std::cmp::Ordering::Equal, Some(e)) => (
                Self::trim(&core, notrim_source, 0, Some(e - 1))?,
                Self::trim(&core, notrim_encoded, 0, Some(e - 1))?,
            ),
            _ => (notrim_source, notrim_encoded),
        };
        let (source, encoded) = if self.every > NonZeroUsize::new(1).unwrap() {
            (
                Self::select_every(&core, source, &self.every)?,
                Self::select_every(&core, encoded, &self.every)?,
            )
        } else {
            (source, encoded)
        };
        let (source, encoded) = (
            Self::bicubic(&core, source, self.width, self.height)?,
            Self::bicubic(&core, encoded, self.width, self.height)?,
        );
        let (s_info, e_info) = (source.info(), encoded.info());
        let num_frames = s_info.num_frames as usize;
        if num_frames != e_info.num_frames as usize {
            let name = self
                .encoded
                .file_name()
                .unwrap()
                .to_string_lossy()
                .to_string();
            return Err(format!(
                "Frame mismatch detected! Source: {num_frames} frames, {name}: {} frames",
                e_info.num_frames
            ));
        }
        let score_node = Arc::new(Mutex::new(match self.method.as_str() {
            "ssimu2_vszip" => Self::vszip(&core, &source, &encoded),
            "ssimu2_vship" => Self::ssimu2_vship(&core, &source, &encoded),
            "butter_vship" => Self::butter_vship(&core, &source, &encoded),
            _ => unreachable!(),
        }?));
        let scores = Arc::new(Mutex::new(Vec::new()));
        let every = self.every.get();

        for _ in 0..self.threads.get() {
            let (score_node, sender) = (Arc::clone(&score_node), sender.clone());
            let scores = Arc::clone(&scores);
            let metric = self.metric.clone();
            let method = self.method.clone();
            thread::spawn(move || {
                loop {
                    let mut scores = scores.lock().unwrap();
                    let mut frame_num = scores.len();
                    if frame_num >= num_frames {
                        break;
                    }
                    while scores.contains(&frame_num) {
                        frame_num += 1;
                    }
                    scores.push(frame_num);
                    drop(scores);
                    let node = score_node.lock().unwrap();
                    let frame = node.get_frame(frame_num as i32).unwrap();
                    let props = frame.properties().unwrap();
                    let score = match metric.as_str() {
                        "ssimulacra2" => {
                            let mut metrics = FrameMetrics::default();
                            let score = props
                                .get_float(KStr::from_cstr(cstr!("_SSIMULACRA2")), 0)
                                .unwrap();
                            if method.as_str() == "ssimu_vszip" {
                                let _ = metrics.ssimulacra2.insert(SSIMULACRA2Metrics {
                                    ssimu2_vszip: Some(SSIMULACRA2Data {
                                        ssimulacra2: ScoreData::Number(score),
                                    }),
                                    ..Default::default()
                                });
                            } else {
                                let _ = metrics.ssimulacra2.insert(SSIMULACRA2Metrics {
                                    ssimu2_vship: Some(SSIMULACRA2Data {
                                        ssimulacra2: ScoreData::Number(score),
                                    }),
                                    ..Default::default()
                                });
                            }
                            metrics
                        }
                        "butteraugli" => {
                            let mut metrics = FrameMetrics::default();
                            let norm2 = props
                                .get_float(KStr::from_cstr(cstr!("_BUTTERAUGLI_2Norm")), 0)
                                .unwrap();
                            let norm3 = props
                                .get_float(KStr::from_cstr(cstr!("_BUTTERAUGLI_3Norm")), 0)
                                .unwrap();
                            let infnorm = props
                                .get_float(KStr::from_cstr(cstr!("_BUTTERAUGLI_INFNorm")), 0)
                                .unwrap();
                            let _ = metrics.butteraugli.insert(ButteraugliMetrics {
                                butter_vship: ButteraugliData {
                                    norm2: ScoreData::Number(norm2),
                                    norm3: ScoreData::Number(norm3),
                                    infnorm: ScoreData::Number(infnorm),
                                },
                            });
                            metrics
                        }
                        _ => unreachable!(),
                    };
                    sender
                        .send(MetricResult {
                            frame: frame_num * every,
                            total_frames: Some(num_frames as usize),
                            score: (score),
                            message: None,
                        })
                        .unwrap();
                }
            });
        }
        Ok(())
    }
    fn calc_metrics(&self, sender: &Sender<MetricResult>) -> Result<(), String> {
        let abs_src = abs(&self.source).map_err(|e| e.to_string())?;
        let abs_dst = abs(&self.encoded).map_err(|e| e.to_string())?;
        let start = if self.start > 0 { self.start - 1 } else { 0 };
        let mut frame = if self.start > 0 { self.start - 1 } else { 0 };
        #[rustfmt::skip]
        let proc = Command::new("turbo-metrics").args([
                "-m", &self.metric,
                "--every", &self.every.to_string(),
                "--skip", &start.to_string(),
                "--frames", &self.end.as_ref().unwrap_or(&0).to_string(),
                "--output", "json-lines",
                &abs_src.to_string_lossy().to_string(),
                &abs_dst.to_string_lossy().to_string(),
            ])
            .stdout(Stdio::piped()).stderr(Stdio::piped())
            .spawn().map_err(|e| e.to_string())?;
        let stdout = BufReader::new(proc.stdout.unwrap());
        let stderr = BufReader::new(proc.stderr.unwrap());
        let stderr_sender = sender.clone();
        let stdout_sender = sender.clone();
        thread::spawn(move || {
            stderr.lines().filter_map(|l| l.ok()).for_each(|l| {
                let json: HashMap<String, Value> = serde_json::from_str(&l).unwrap();
                if !json.contains_key("INFO") {
                    stderr_sender
                        .send(MetricResult {
                            message: Some(Message {
                                msg: serde_json::to_string(&json).unwrap(),
                                ..Default::default()
                            }),
                            ..Default::default()
                        })
                        .unwrap();
                }
            });
        });
        let metric = self.metric.clone();
        let every = self.every.get();
        thread::spawn(move || {
            stdout.lines().filter_map(|l| l.ok()).for_each(|l| {
                let json: HashMap<String, Value> = serde_json::from_str(&l).unwrap();
                if !json.contains_key(&metric) {
                    return;
                }
                frame += 1;
                let score = json.get(&metric).unwrap().as_f64().unwrap();
                let mut result = FrameMetrics::default();
                match metric.as_str() {
                    "ssimulacra2" => {
                        let _ = result.ssimulacra2.insert(SSIMULACRA2Metrics {
                            ssimu2_vszip: None,
                            ssimu2_vship: None,
                            ssimu2_turbo: Some(SSIMULACRA2Data {
                                ssimulacra2: ScoreData::Number(score),
                            }),
                        });
                    }
                    "psnr" => {
                        let _ = result.psnr.insert(PSNRMetrics {
                            psnr_turbo: PSNRData {
                                psnr: ScoreData::Number(score),
                            },
                        });
                    }
                    "ssim" => {
                        let _ = result.ssim.insert(SSIMMetrics {
                            ssim_turbo: SSIMData {
                                ssim: ScoreData::Number(score),
                            },
                        });
                    }
                    "msssim" => {
                        let _ = result.msssim.insert(MSSSIMMetrics {
                            msssim_turbo: MSSSIMData {
                                msssim: ScoreData::Number(score),
                            },
                        });
                    }
                    _ => unreachable!(),
                }
                stdout_sender
                    .send(MetricResult {
                        frame: frame * every,
                        total_frames: None,
                        score: result,
                        message: None,
                    })
                    .unwrap();
            });
        });
        Ok(())
    }
    pub fn run(&self, sender: &Sender<MetricResult>, progress: &ProgressBar) -> Result<(), String> {
        if !self.method.contains("turbo") {
            self.calc_native(&sender, progress)
        } else {
            self.calc_metrics(&sender)
        }
    }
}
