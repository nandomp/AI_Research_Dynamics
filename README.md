# Research Community Dynamics behind Popular AI Benchmarks

The widespread use of experimental benchmarks in AI research has created new competition and collaboration dynamics that are still poorly understood. In this work we provide an innovative methodology to explore this dynamics and analyse the way different entrants in these competitions, from academia to tech giants, behave and react depending on their own or others’ achievements. 

* We perform an analysis of twenty five popular benchmarks in AI from Papers With Code, with around two thousand result entries overall, connected with their underlying research papers. 

* We identify links between researchers and institutions (i.e., communities) beyond the standard co-authorship relations, and we explore a series of hypotheses
about their behaviour as well as some aggregated results in terms of activity, performance jumps and efficiency. 

* We detect and characterise the dynamics of research communities at different levels of abstraction, including organisation, affiliation, trajectories, results and activity. 

*Associated paper under revision.*

## CODE

* **benchmark_analyzer.ipynb**: Code for extracting benchmark data from Papers With Code and affiliation data from Scinapse

* **SOTAfront_plots.R**: Code for ploting the results in the paper.

* **hypotheses_testing.R**: Code for testing the hypotheses in the paper.


## DATA

(Folder Data/) Papers, authors, results, community memberships, SOTA jumps and dates. 

* Image Classification
    * ImageNet
    * CIFAR-100

* Semantic Segmentation
   * Cityscapes
   * Pascal VOC 2012 test

* Object Detection
   * COCO test-dev
   * COCO Minival 

* Image Generation
   * CIFAR-10

* Pose Estimation
   * MPPII Human Pose

* Action Recognition
   * Videos on UCF101
   * Videos on HMDB-51

* Image Super-Resolution
   * Set5 - 4x upscaling 

* Machine Translation
   * WMT2014 Eng-Ger
   * WMT2014 Eng-Fre

* Question Answering
   * SQuAD1.1 
   * WikiQA

* Language Modelling
   * Penn Treebank
   * enwik8

* Sentiment Analysis
   * SST-2 Binary classification 
   * IMDb

* Named Entity Recognition
   * CoNLL 2003 (English) 
   * Ontonotes v5 (English)

* Speech Recognition
   * LibriSpeech test-clean

* Link Prediction
   * WN18RR

* Atari
   * Atari 2600 Montezuma's Revenge 
   * Atari 2600 Space Invaders 

## BASELINES

(/Baselines folder) High quality plots comparing community, affiliation and author grouping dynamics (progress in accuracy over time) for all the benchmarks analysed.

## FIGURES

(/Figures folder) High quality plots showing community grouping dynamics (progress in accuracy over time) for all the benchmarks analysed.



