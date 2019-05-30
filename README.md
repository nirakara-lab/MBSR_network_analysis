# MBSR_network_analysis
[![DOI](https://zenodo.org/badge/189476141.svg)](https://zenodo.org/badge/latestdoi/189476141)

## Does mindfulness change the mind? A psychonectome perspective based on Network Analysis.
Pablo Roca, Gustavo G. Diez, Nazareth Castellanos, Carmelo Vazquez

The aim of this study was to examine the MBSR’s (Mindfulness Based Stress Reduction) impact on the network dynamics between mindfulness, compassion, well-being, psychological distress and emotional and cognitive control constructs, and how these constructs are reorganized after the intervention. Participants were asked to complete online (via Qualtrics software) the set of questionnaires described in detail below. The online assessment was completed the week before starting of the program (baseline assessment) and during the week after the end of the MBSR (post-assessment). The MBSR program consisted of 32-hour training during eight weeks, including a 3-hour initial orientation session, 7 weekly 2.5-hour of face-to-face sessions, an 8-hour intensive day of practice, 45 minutes of daily home formal and informal practices and a final 3.5-hour session. Training was conducted in groups of 20-30 participants. During the program, different mindfulness practices are performed, including focused attention on the breath, open monitoring of awareness in body-scanning, prosocial meditation (i.e. loving kindness and compassion) and gentle yoga.

## Sample description
A sample of 258 adults, enrolled in a standardized 8-week MBSR program between Apriland December 2017, were invited to participate in this research (75.2% accepted to participateand fulfilled the inclusion criteria described below).  After applying all exclusion criteria seeFigure 1), data from a total of 182 individuals were included in all analyses. Participants meanage was 43.7 (S.D.=9.77), 70.9% women, 95.7 % had higher education, 40.1% married, 72.5%employed, 13,7% physical illness, 56% previous meditation experience and the meditation yearsaverage was 4.19 (S.D.=5.72).  The research study was approved by the university ethicscommittee prior to participant recruitment.

## Constructs and instruments of study
### Mindfulness
-	Five-Facet Mindfulness Questionnaire-Short Form (FFMQ, 20 items [α = .87]; [71]). It  includes five component skills of of mindfulness: observing, describing, acting with awareness, non-judging of inner experience, and non-reactivity to inner experience
-	Non-Attachment Scale (NAS, 30 items [α = .93]; [72]). It measures the absence of fixation on thoughts, images, or sensory inputs, as well as an absence of internal pressure to get, hold, avoid, or change circumstances or experiences.
-	Experiences Questionnaire (EQ, 11 items [α = .89]; [73]). It assesses the ability to observe one’s thoughts and feelings in a detached manner.
-	Multidimensional assessment of interoceptive awareness (MAIA, 32 items [α = .94];  [74]). It measures interoceptive body awareness.

### Compassion
-	Self-Compassion Scale-Short Form (SCS-SF, 12 items [α = .88]; [75]). It measures three components of compassion to oneself: self-kindness, common humanity, and mindfulness.
-	Compassion Scale (CSP, 24 items [α = .86]; [76]). It assesses compassion to others, through the following components: kindness, indifference, common humanity, separation, mindfulness and disengagement.
-	Interpersonal Reactivity Index (IRI, 14 items [α = .77]; [77]). It measures empathy towards others. In this study only the Empathic Concern subscale was included.
### Psychological well-being
-	Satisfaction With Life Scale (SWLS, 5 items [α = .87]; [78]). This is a measure of global life satisfaction.
-	Life Orientation Test–Revised (LOT-R, 10 items [α = .67]; [79]). It measures dispositional optimism.
-	Pemberton Happiness Index (PHI, 11 items [α = .91]; [80]). A measure that includes both hedonic and eudaimonic components of psychological well-being.
### Psychological Distress
-	Depression Anxiety Stress Scales (DASS-21, 21 items [α = .92]; [81]). It measures symptoms of depression, anxiety and stress.
### Emotional and cognitive control
-	White Bear Supression Inventory (WBSI, 10 items [α = .89]; [82]). It measures unwanted intrusive thoughts and thought suppression.
-	Ruminative Response Style (RRS, 22 items [α = .92]; [83]). It assesses an excessive focus on causes and consequences of depressive symptoms. It includes two factors: reflection and brooding.
-	Emotion Regulation Questionnaire (ERQ, 10 items [α = .76]; [84]). It measures two emotional regulation strategies: reappraisal and suppression.
-	Attentional Control Scale (ACS, 20 items [α = .84]; [85]). It assesses perceived ability in executive control over attention.

### Dataframe

*network_pre*: 25 variables. Online assessment was completed the week before starting MBSR.
- **mindfulness** ="FFMQ_O","FFMQ_D","FFMQ_A","FFMQ_J","FFMQ_R" (5 subscales of Five-Facet Mindfulness Questionnaire-Short Form), "NAS","EQ","MAIA".
- **compassion** = "SCS_A","SCS_H","SCS_M"(3 subscales of Self-Compassion Scale-Short Form),"CSP","IRI_E" (empathy subscale of Interpersonal Reactivity Index),
- **psychologycal wellbeing** = "SWLS","LOT","PHI",
- **psychologycal distress** = "DASS_S","DASS_D","DASS_A"(3 subsclaes of Depression Anxiety Stress Scales)
- **Emotional and cognitive control** = "WBSI", "RRS_B", "RRS_R"(2 subsclale of Ruminative Response Style), "ERQ_R", "ERQ_S" (2 subscales of Emotion Regulation Questionnaire), "ACS"

*network_post*: 25 variables. Online assessment was completed during the week after the end of the MBSR.
- **mindfulness** ="FFMQ_O","FFMQ_D","FFMQ_A","FFMQ_J","FFMQ_R" (5 subscales of Five-Facet Mindfulness Questionnaire-Short Form), "NAS","EQ","MAIA".
- **compassion** = "SCS_A","SCS_H","SCS_M"(3 subscales of Self-Compassion Scale-Short Form),"CSP","IRI_E" (empathy subscale of Interpersonal Reactivity Index),
- **psychologycal wellbeing** = "SWLS","LOT","PHI",
- **psychologycal distress** = "DASS_S","DASS_D","DASS_A"(3 subsclaes of Depression Anxiety Stress Scales)
- **Emotional and cognitive control** = "WBSI", "RRS_B", "RRS_R"(2 subsclale of Ruminative Response Style), "ERQ_R", "ERQ_S" (2 subscales of Emotion Regulation Questionnaire), "ACS"

### Script of network analysis in R
*MBSR_network_analysis.R* contains the analysis done in the paper and the supplementary material.
