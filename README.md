### Data and script that accompany the manuscript titled "**Echoes of self: Understanding acoustic structure and informational content in common marmoset (_Callithrix jacchus_) phee sequences**".

&nbsp;

#### **Abstract**

Communication in social animals relies on acoustic cues, yet many species possess limited vocal repertoires. To compensate, animals often produce vocalizations in sequences, potentially enhancing the diversity of transmitted information. The significance of repeated elements within these sequences remains poorly understood. Here, we investigated the spectro-temporal structure of elements within common marmoset (Callithrix jacchus) phees – long-distance contact calls, often produced in sequences. Employing machine learning techniques (random forests) and linear mixed effects models, we explored how elements varied based on their position within sequences and assessed their ability to encode identity and sex information. We examined similarities between elements occupying the same position in different sequences. Our results reveal structural differences within and between sequences, with variations in phees at distinct positions within same sequences and similarities between first elements of distinct sequences. All phees encoded caller identity and sex information, with varying accuracy across different sequence positions – higher encoding of sex information at the beginning of the sequence and a greater emphasis on identity later on. These findings suggest that repeat sequences may be more functionally diverse than previously thought, prompting future research into these structures outside of the contexts of robust encoding of messages.

&nbsp;

<details>
  <summary>Repository Contents</summary>

- [formal_analysis.R](https://github.com/kristin-mesh/Echoes-of-self---Understanding-acoustic-structure-and-informational-content-in-common-marmoset/blob/master/formal_analysis.R) - R script used to perform formal analysis as described in the Methods section of the manuscript;
- [raw_data.xlsx](https://github.com/kristin-mesh/Echoes-of-self---Understanding-acoustic-structure-and-informational-content-in-common-marmoset/blob/master/raw_data.xlsx) - raw data in .xlsx file format; Sheet 1 contains all data points and Sheet 2 contains description of variables;
- [rds_data.rds](https://github.com/kristin-mesh/Echoes-of-self---Understanding-acoustic-structure-and-informational-content-in-common-marmoset/blob/master/rds_data.rds) - data in .rds file format; here, the ID, sex, element, el_type, el_no, date, file_number, linear, curve, shape, call_ref variables are converted to factors;

</details>

&nbsp;

To run the script please use the latest versions of R and RStudio.
