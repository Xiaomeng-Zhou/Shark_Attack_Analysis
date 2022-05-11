# Exploring Shark Attacks Across the World

### Authors: Sean Oberer, Lenka Raslova, and Xiaomeng Zhou

This app was created as a final project for Chase Romano's Visual Analytics class in the Data Science and Business Analytics Master's program at the University of North Carolina at Charlotte. The app is to provide medical personnel, shark behaviorists, lifesavers, and the media with meaningful information resulting from the scientific forensic examination of shark accidents.

To view the live app click [here](https://xiaomengdsba.shinyapps.io/Shark_attack_analysis/).

<hr>

### Reproduce the app
 Follow these steps if you would like to download the project files and run it instead of going to the link above: <h5>
<ol>
  <li>Download all of the items expect *Preprocess_Geocoding* folder in the repository</li>
  <li>Open <ins>my dashboard.Rproj</ins> in R Studio</li>
  <li>Open the file named <ins>dashboard_final.R</ins></li>
  <li>Click <ins>Run App</ins> in the upper right corner of the code panel</li>
</ol>
  
<hr>

### Data Preprocess --Geocoding

The highlight of the project is to geocode the shark attack location information and
create an interactive map. 

The original dataset came in with three columns: ***Country***, ***Area*** and ***Location*** to mark the shark attack location information. It did not include the lantitude and longtitude when shark attack happened.

In order to create an interactive map, I first combined  ***Country***, ***Area*** and ***Location*** as the shark attack address, then used the ***tidygeocoder*** package to geocode
the address.  

The geocoding code can be accessed in the folder***Preprocess_Geocoding*** in the repository.


### Data Sources

You may access the data from the Kaggle website using the link below:
<ul>
  <li>https://www.kaggle.com/datasets/felipeesc/shark-attack-dataset</li>
 </ul>
 