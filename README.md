# Cyclistic Bike-Share Analysis

## Overview
Cyclistic launched its bike-share program in 2016 and has since grown to a fleet of 5,824 geotracked bicycles across 692 stations in Chicago, allowing bikes to be picked up at one station and returned to any other within the network. The company offers flexible pricing plans, including single-ride passes, full-day passes, and annual memberships, categorizing users as casual riders and annual members.

Financial analysis indicates that annual members are more profitable than casual riders, making membership growth essential for Cyclistic’s future. Rather than targeting only new customers, the company aims to convert existing casual riders into annual members by better understanding how rider types differ. To support this goal, Cyclistic analyzes historical bike trip data to identify trends that can inform effective marketing strategies.

## Business Problem
Cyclistic wants to grow revenue by converting casual riders into annual members, as members are more profitable. To support targeted marketing strategies, the company needs to understand how annual members and casual riders use Cyclistic bikes differently by analyzing historical trip data.

## Role and Responsibility
As a junior data analyst on Cyclistic’s marketing analytics team, I was responsible for uncovering insights into how casual riders and annual members use Cyclistic bikes differently. My role included data cleaning, analysis, visualization, and communicating findings to support strategies aimed at converting casual riders into annual members.

## Tools Used
- R (data cleaning & analysis)
- Tableau (data visualization & dashboarding)

## Key Insights
- Members dominate weekday usage with short trips.
- Casual riders take longer rides on weekends.
- Clear opportunity for membership conversion.

## Dataset Information
- The dataset used in this project is Cyclistic’s historical bike-share trip data.
- Due to file size limitations, the raw and cleaned datasets are not included in this repository.
- The data is publicly available and can be downloaded from the original source.
- After downloading, the data was cleaned and prepared using R before analysis and visualization.
- Source: https://divvy-tripdata.s3.amazonaws.com/index.html

## Dashboard
🔗 Tableau Public Link: https://public.tableau.com/views/Cyclisticbike-shareanalysis-Capstone/Dashboard1?:language=en-US&:sid=&:redirect=auth&:display_count=n&:origin=viz_share_link

<div class='tableauPlaceholder' id='viz1766646265438' style='position: relative'><noscript><a href='#'><img alt=' ' src='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;Cy&#47;Cyclisticbike-shareanalysis-Capstone&#47;Dashboard1&#47;1_rss.png' style='border: none' /></a></noscript><object class='tableauViz'  style='display:none;'><param name='host_url' value='https%3A%2F%2Fpublic.tableau.com%2F' /> <param name='embed_code_version' value='3' /> <param name='site_root' value='' /><param name='name' value='Cyclisticbike-shareanalysis-Capstone&#47;Dashboard1' /><param name='tabs' value='yes' /><param name='toolbar' value='yes' /><param name='static_image' value='https:&#47;&#47;public.tableau.com&#47;static&#47;images&#47;Cy&#47;Cyclisticbike-shareanalysis-Capstone&#47;Dashboard1&#47;1.png' /> <param name='animate_transition' value='yes' /><param name='display_static_image' value='yes' /><param name='display_spinner' value='yes' /><param name='display_overlay' value='yes' /><param name='display_count' value='yes' /><param name='language' value='en-US' /></object></div>                <script type='text/javascript'>                    var divElement = document.getElementById('viz1766646265438');                    var vizElement = divElement.getElementsByTagName('object')[0];                    if ( divElement.offsetWidth > 800 ) { vizElement.style.minWidth='1366px';vizElement.style.maxWidth='100%';vizElement.style.minHeight='818px';vizElement.style.maxHeight=(divElement.offsetWidth*0.75)+'px';} else if ( divElement.offsetWidth > 500 ) { vizElement.style.minWidth='1366px';vizElement.style.maxWidth='100%';vizElement.style.minHeight='818px';vizElement.style.maxHeight=(divElement.offsetWidth*0.75)+'px';} else { vizElement.style.width='100%';vizElement.style.minHeight='1800px';vizElement.style.maxHeight=(divElement.offsetWidth*1.77)+'px';}                     var scriptElement = document.createElement('script');                    scriptElement.src = 'https://public.tableau.com/javascripts/api/viz_v1.js';                    vizElement.parentNode.insertBefore(scriptElement, vizElement);                </script>


## Recommendations
- Weekend membership promotions
- Flexible membership plans
- Targeted marketing for casual riders
