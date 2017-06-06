# UVA Blog Replication

The code behind blog posts I have authored for the University of Virginia's Weldon Cooper Center Demographics Research Group is located here. It is my intention that these posts are fully replicable, excepting the cases where replacing credentials and API keys is necessary to download data from primary sources.

Each post roughly follows my [personal project tempalte](https://github.com/StephenHolzman/project-template). For most purposes the post folders on github only have an 'analysis' folder with a single R script. Scripts in the 'analysis' folder will download data and automatically create other folders as needed, most likely 'input' and 'output'. In cases where Blog posts are related and rely on the same data, there will be multiple scripts.

### Getting Started

I suggest cloning or downloading the full repository. It only contains text files of small size. Most projects are [RStudio](https://www.rstudio.com/) based, so make sure you have it and the latest version of R installed.

To replicate a post, navigate to the directory and open the .Rproj file. This should set the directory properly, then run scripts located in the analysis folder and watch the data download and figures from the blog post generate. You may have to replace some credentials or API keys and make sure that dependency libraries are installed to get everything going.