Seminar paper title: Online Outlier Exploration Over Large Datasets (like the original paper title)

 * Motivation
    * Outlier Definitions
        * Introduce Distance Based Outliers
        * Briefly discuss other outlier definitions
        * Motivate why Distance Based Outliers are used
    * State of the Art for Distance-Based Outliers
        * Introduce the nested-loop algorithm and its performance problem
        * Explain that it can be reduced from the idea of k-nearest neighbors
        * Briefly introduce the kd-tree algorithm (or some other n*log(n) algorithm)
        * Mention approximate nearest neighbors as alternative
        * Mention the DOLPHIN System
    * Considerations For A Better Outlier Detection System
        * Introduce Online/Offline Idea of the ONION System
        * Motivate and Define Outlier-Centric Parameter Space Exploration
        * Motivate and Define Comparative Outliers
 * Overview of the ONION System (I might cut this chapter and integrate it into the  following chapter.
    * Offline Phase
        * Three tiered architecture of offline phase
        * Give rough intuition of what improvement each step of the offline phase brings
    * Online Phase
        * Give brief overview of what is done to handle the different operations
 * Offline Phase
    * O-Space
        * Define what O-Space is
        * Give an algorithm for its construction from a K-nearest neighbors query
    * P-Space
        * Define what P-Space is
        * Give algorithm for its construction
    * D-Space
        * Define what D-Space is
        * Give algorithm for its construction
 * Online Phase
    * Outlier Detection
        * Explain how outlier detection can be carried out using the three Spaces
    * Comparative Outlier Analytics
        * Explain how COO can be carried out using the three Spaces
    * Outlier-Centric Parameter Space Exploration
        * Explain how PSE can be carried out using the three Spaces
 * Evaluation
    * Data Sets
        * Briefly introduce data sets used for evaluation
    * Offline Phase
        * Vary the parameters and give runtime data
    * Online Phase
        * Vary the parameters and give runtime data
 * Summary
    * Sum up contributions of paper
    * Specifically also compare to e.g. the DOLPHIN system
    * Criticize the following issues:
        * Initial parameter Settings can not be chosen very broadly to avoid producing too many ocs.
        * D-Space Construction is very costly
        * As a consequence, the offline phase may have to be performed several times in some typical use cases
     
