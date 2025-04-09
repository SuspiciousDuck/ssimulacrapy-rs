use crate::{macros::lossy_to_string, utils::calculate_scores};
use serde::{Deserialize, Serialize};
use std::{collections::HashMap, num::NonZeroUsize, path::PathBuf};

#[derive(Serialize, Deserialize)]
pub struct EncodedData {
    pub file: PathBuf,
    pub scores: ScoreResult,
    pub start_frame: usize,
    pub end_frame: Option<usize>,
    pub every: NonZeroUsize,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct MetricStats {
    pub average: f64,
    pub standard_deviation: f64,
    pub median: f64,
    pub percentile_5: f64,
    pub percentile_95: f64,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(untagged)]
pub enum ScoreData {
    Number(f64),
    MetricStats(MetricStats),
}

// really this should be a TryFrom, but i dont really care
impl From<&ScoreData> for f64 {
    fn from(value: &ScoreData) -> Self {
        match value {
            ScoreData::Number(num) => *num,
            _ => panic!("ScoreData is not an f64!"),
        }
    }
}

impl TryFrom<&ScoreData> for MetricStats {
    type Error = String;
    fn try_from(value: &ScoreData) -> Result<Self, Self::Error> {
        match value {
            ScoreData::MetricStats(metric_stats) => Ok(metric_stats.clone()),
            _ => Err("ScoreData is not a MetricStats!".to_string()),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SSIMULACRA2Data {
    pub ssimulacra2: ScoreData,
}
impl TryFrom<Option<MetricStats>> for SSIMULACRA2Data {
    type Error = String;
    fn try_from(value: Option<MetricStats>) -> Result<Self, Self::Error> {
        value
            .map(|d| Self {
                ssimulacra2: ScoreData::MetricStats(d),
            })
            .ok_or("MetricStats is None!".to_string())
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Default)]
pub struct SSIMULACRA2Metrics {
    pub ssimu2_vszip: Option<SSIMULACRA2Data>,
    pub ssimu2_vship: Option<SSIMULACRA2Data>,
    pub ssimu2_turbo: Option<SSIMULACRA2Data>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ButteraugliData {
    #[serde(rename = "2Norm")]
    pub norm2: ScoreData,
    #[serde(rename = "3Norm")]
    pub norm3: ScoreData,
    #[serde(rename = "INFNorm")]
    pub infnorm: ScoreData,
}
impl From<(MetricStats, MetricStats, MetricStats)> for ButteraugliData {
    fn from(value: (MetricStats, MetricStats, MetricStats)) -> Self {
        Self {
            norm2: ScoreData::MetricStats(value.0),
            norm3: ScoreData::MetricStats(value.1),
            infnorm: ScoreData::MetricStats(value.2),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ButteraugliMetrics {
    pub butter_vship: ButteraugliData,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PSNRData {
    pub psnr: ScoreData,
}
impl From<MetricStats> for PSNRData {
    fn from(value: MetricStats) -> Self {
        Self {
            psnr: ScoreData::MetricStats(value),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PSNRMetrics {
    pub psnr_turbo: PSNRData,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SSIMData {
    pub ssim: ScoreData,
}
impl From<MetricStats> for SSIMData {
    fn from(value: MetricStats) -> Self {
        Self {
            ssim: ScoreData::MetricStats(value),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SSIMMetrics {
    pub ssim_turbo: SSIMData,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct MSSSIMData {
    pub msssim: ScoreData,
}
impl From<MetricStats> for MSSSIMData {
    fn from(value: MetricStats) -> Self {
        Self {
            msssim: ScoreData::MetricStats(value),
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct MSSSIMMetrics {
    pub msssim_turbo: MSSSIMData,
}

#[derive(Serialize, Deserialize, Default, Debug, Clone)]
pub struct FrameMetrics {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ssimulacra2: Option<SSIMULACRA2Metrics>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub butteraugli: Option<ButteraugliMetrics>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub psnr: Option<PSNRMetrics>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ssim: Option<SSIMMetrics>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub msssim: Option<MSSSIMMetrics>,
}

pub enum MetricOptions<'a> {
    SSIMULACRA2(&'a SSIMULACRA2Metrics),
    Butteraugli(&'a ButteraugliMetrics),
    PSNR(&'a PSNRMetrics),
    SSIM(&'a SSIMMetrics),
    MSSSIM(&'a MSSSIMMetrics),
}
impl<'a> TryFrom<(&MetricOptions<'a>, &String)> for MetricStats {
    type Error = String;
    fn try_from(value: (&MetricOptions<'a>, &String)) -> Result<Self, Self::Error> {
        match (value.0, value.1.as_str()) {
            (MetricOptions::SSIMULACRA2(d), "ssimu2_vszip") => {
                MetricStats::try_from(&d.ssimu2_vszip.as_ref().unwrap().ssimulacra2)
            }
            (MetricOptions::SSIMULACRA2(d), "ssimu2_vship") => {
                MetricStats::try_from(&d.ssimu2_vship.as_ref().unwrap().ssimulacra2)
            }
            (MetricOptions::SSIMULACRA2(d), "ssimu2_turbo") => {
                MetricStats::try_from(&d.ssimu2_turbo.as_ref().unwrap().ssimulacra2)
            }
            (MetricOptions::Butteraugli(_), "butter_vship") => {
                Err("Wont return Butteraugli averages".to_string())
            }
            (MetricOptions::PSNR(d), "psnr_turbo") => MetricStats::try_from(&d.psnr_turbo.psnr),
            (MetricOptions::SSIM(d), "ssim_turbo") => MetricStats::try_from(&d.ssim_turbo.ssim),
            (MetricOptions::MSSSIM(d), "msssim_turbo") => {
                MetricStats::try_from(&d.msssim_turbo.msssim)
            }
            _ => unreachable!(),
        }
    }
}
macro_rules! metric_impl {
    ($ty:ty, $ident:ident) => {
        impl<'a> TryFrom<&MetricOptions<'a>> for $ty {
            type Error = String;
            fn try_from(value: &MetricOptions<'a>) -> Result<Self, Self::Error> {
                match value {
                    MetricOptions::$ident(x) => Ok(x),
                    _ => Err(format!(
                        "MetricOptions was not a {} value!",
                        stringify!($ident)
                    )),
                }
            }
        }
    };
}
metric_impl!(&'a SSIMULACRA2Metrics, SSIMULACRA2);
metric_impl!(&'a ButteraugliMetrics, Butteraugli);
metric_impl!(&'a PSNRMetrics, PSNR);
metric_impl!(&'a SSIMMetrics, SSIM);
metric_impl!(&'a MSSSIMMetrics, MSSSIM);

impl FrameMetrics {
    pub fn get(&self, query: &String) -> Option<MetricOptions> {
        match query.as_str() {
            "ssimulacra2" => self
                .ssimulacra2
                .as_ref()
                .map(|d| MetricOptions::SSIMULACRA2(d)),
            "butteraugli" => self
                .butteraugli
                .as_ref()
                .map(|d| MetricOptions::Butteraugli(d)),
            "psnr" => self.psnr.as_ref().map(|d| MetricOptions::PSNR(d)),
            "ssim" => self.ssim.as_ref().map(|d| MetricOptions::SSIM(d)),
            "msssim" => self.msssim.as_ref().map(|d| MetricOptions::MSSSIM(d)),
            _ => unreachable!(),
        }
    }
}

#[derive(Serialize, Deserialize, Default)]
pub struct ScoreResult {
    pub frame: HashMap<usize, FrameMetrics>,
    #[serde(rename = "final")]
    pub _final: FrameMetrics,
}

#[derive(Serialize, Deserialize, Default)]
pub struct Source {
    pub encoded: HashMap<String, EncodedData>,
}

#[derive(Serialize, Deserialize, Default)]
pub struct ScoresOuter {
    pub source: HashMap<String, Source>,
}

pub struct Scores {
    pub scores_path: Option<PathBuf>,
    pub scores: ScoresOuter,
}
impl Scores {
    pub fn new(scores_path: Option<&PathBuf>) -> Self {
        Self {
            scores_path: scores_path.cloned(),
            scores: ScoresOuter::default(),
        }
    }

    /// Add a video to the scores file
    pub fn add_video(
        &mut self,
        distorted_files: &Vec<PathBuf>,
        distorted_clip_hash: &HashMap<String, String>,
        source_file: &PathBuf,
        args: &crate::Args,
    ) {
        let source_name = lossy_to_string!(source_file.file_name().unwrap());
        if !self.scores.source.contains_key(&source_name) {
            self.scores
                .source
                .insert(source_name.clone(), Source::default());
        }

        for encoded_file in distorted_files {
            let encoded_filename = lossy_to_string!(encoded_file.file_name().unwrap());
            let clip_hash = distorted_clip_hash[&encoded_filename].clone();
            let encoded = &mut self.scores.source.get_mut(&source_name).unwrap().encoded;

            if !encoded.contains_key(&clip_hash) {
                encoded.insert(
                    clip_hash.clone(),
                    EncodedData {
                        file: encoded_file.clone(),
                        scores: ScoreResult::default(),
                        start_frame: args.start,
                        end_frame: Some(*args.end.as_ref().unwrap_or(&0)),
                        every: args.every,
                    },
                );
            }
        }
    }
    
    /// Add a frame to the scores file
    pub fn add_frame(
        &mut self,
        source_file: &PathBuf,
        clip_hash: &String,
        frame: usize,
        score: FrameMetrics,
    ) {
        let source_name = lossy_to_string!(source_file.file_name().unwrap());
        let source = self.scores.source.get_mut(&source_name).unwrap();
        let encoded = source.encoded.get_mut(clip_hash).unwrap();
        encoded.scores.frame.insert(frame, score);
    }

    /// Check if a metric exists for a video
    pub fn exists(&self, source_file: &PathBuf, clip_hash: &String, metric: &String) -> bool {
        let source_name = lossy_to_string!(source_file.file_name().unwrap());
        self.scores.source.contains_key(&source_name)
            && self.scores.source[&source_name]
                .encoded
                .contains_key(clip_hash)
            && self.scores.source[&source_name].encoded[clip_hash]
                .scores
                ._final
                .get(&metric)
                .is_some()
    }

    /// Get the number of frames in a distorted video
    pub fn get_frames(&self, source_file: &PathBuf, clip_hash: &String) -> usize {
        let source_name = lossy_to_string!(source_file.file_name().unwrap());
        self.scores.source[&source_name].encoded[clip_hash]
            .scores
            .frame
            .len()
    }

    /// Get the scores for a video
    pub fn get_scores(&self, source_file: &PathBuf, clip_hash: &String) -> Vec<&FrameMetrics> {
        let source_name = lossy_to_string!(source_file.file_name().unwrap());
        self.scores.source[&source_name].encoded[clip_hash]
            .scores
            .frame
            .values()
            .collect()
    }

    /// Get the scores for a video
    pub fn get_final_scores(&self, source_file: &PathBuf, clip_hash: &String) -> &FrameMetrics {
        let source_name = lossy_to_string!(source_file.file_name().unwrap());
        &self.scores.source[&source_name].encoded[clip_hash]
            .scores
            ._final
    }

    /// Calculate and store final scores for each metric
    pub fn finalize_video(&mut self, source_file: &PathBuf, clip_hash: &String) {
        let source_name = lossy_to_string!(source_file.file_name().unwrap());
        let source = self.scores.source.get_mut(&source_name).unwrap();
        let clip_data = source.encoded.get_mut(clip_hash).unwrap();

        let frame_scores: Vec<&FrameMetrics> = clip_data.scores.frame.values().collect();
        clip_data.scores._final = calculate_scores(frame_scores);
    }

    /// Load the scores file
    pub fn load(&mut self) -> Result<(), String> {
        let scores_path = self.scores_path.as_ref();
        if scores_path.is_none() {
            return Err("Scores path is None! Failed to load file!".to_string());
        } else if scores_path.is_some_and(|p| p.try_exists().is_ok_and(|b| b == false)) {
            return Err("Specified scores path does not exist!".to_string());
        }
        let scores_path = scores_path.unwrap();
        let buffer = std::fs::File::open(scores_path).map_err(|e| e.to_string())?;
        let data: ScoresOuter = serde_json::from_reader(buffer).map_err(|e| e.to_string())?;
        self.scores = data;
        Ok(())
    }

    /// Save the scores file
    pub fn save(&self) -> Result<(), String> {
        let scores_path = self.scores_path.as_ref();
        if scores_path.is_none() {
            return Err("Scores path is None! Failed to save file!".to_string());
        }
        let scores_path = scores_path.unwrap();
        let buffer = std::fs::File::create(scores_path).map_err(|e| e.to_string())?;
        serde_json::to_writer(buffer, &self.scores).map_err(|e| e.to_string())?;
        Ok(())
    }
}
