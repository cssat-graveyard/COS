# The POC Case Outcome Simulator (COS)
The COS is an R and Shiny application for exploring a model of child welfare 
case outcomes through simulation and visualization. It is an instance of the 
[Multinomial Outcome Simulator (MOS)](https://github.com/bwaismeyer/MOS_framework).

The COS can be seen in action 
[here](http://ec2-54-183-103-46.us-west-1.compute.amazonaws.com:3939/COS/).

Specifically, targeted user groups interact with the application by adjusting 
model inputs and then observing as the application simulates likelihoods for 
the four key case outcomes (Reunification, Adoption, Guardianship, and 
Emancipation) based on those inputs.

There are different application "modes" for different user groups, all built 
upon the same application infrastructure but with user interface features and 
visualizations tailored to the user groups.

As of this writing, the available modes are:
* Explore Mode: designed for academics and policy makers, appropriate for 
    observing how outcome likelihoods change across a range of values
* Single Case Mode: designed for case workers, appropriate for simulating the 
    likelihood of outcomes based on inputs for a single case

## Setting Up the AWS EC2 Instance
Currently this application is deployed via 
[Amazon's EC2 service](http://aws.amazon.com/ec2/?sc_channel=PS&sc_campaign=acquisition_US&sc_publisher=google&sc_medium=ec2_b&sc_content=ec2_e&sc_detail=amazon.ec2&sc_category=ec2&sc_segment=53611778562&sc_matchtype=e&sc_country=US&s_kwcid=AL!4422!3!53611778562!e!!g!!amazon.ec2&ef_id=VTlq7QAAAQOLjYDQ:20150511210335:s). 

You can sign in to the [POC AWS here](https://804797177177.signin.aws.amazon.com/console), 
though you'll need proper credentials.

What follows will describe how to setup a (free) micro instance for the COS 
using the EC2 Ubuntu AMI.

### Launching the Instance
First step, we need to create an instance. This guide will assume you have 
access to an Amazon Web Service's (AWS) account and know how to login.

Once you've logged in, follow Amazon's 
[instance launch instructions](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-instance_linux.html)
, noting the following: 
* The guide assumes that you will use the Ubuntu Amazon Machine Image (AMI). 
    If you want to use a different AMI, please be prepared to figure out the 
    correct default user name and shell commands.
* Accept the default security settings for the time being. We'll change those 
    during the next step.
* Make sure you get the private key!

### Authorize Inbound Traffic to the Instance
We have an instance. Now we need to make sure we (and our users) can talk to it. 
By default, AWS gives very broad access privileges. At least initially, this is 
probably fine.

Verify the server's inbound traffic rules by following 
[these directions](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/authorizing-access-to-an-instance.html).

If needed, update the selected security group to allow access via SSH 
(universally or to your specific IP).

### Setup Putty to Access the Instance
Now that the instance is launched and open to SSH, we need to setup an SSH tool. 
We'll use putty.

Follow the directions for 
[installing and configuring putty](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/putty.html).
	
**Note**: You can stop following the directions after you finish the "Starting 
a Putty Session" section.

### Setup a Swapfile
Computing environments low in volatile memory (i.e., RAM) run the risk of 
easily running out of working memory during memory intense operations - such as 
certain R package installations and other operations. The AWS micro instances 
are low-memory computing environments.

To counter this, we assign a special file the instance can use - a swapfile - 
during periods when it runs out of RAM. 

Login into the server via putty and then simply run this code (taken from 
[here](http://serverfault.com/questions/218750/why-dont-ec2-ubuntu-images-have-swap)) 
in the putty console:
```
sudo dd if=/dev/zero of=/var/swapfile bs=1M count=2048 &&
sudo chmod 600 /var/swapfile &&
sudo mkswap /var/swapfile &&
echo /var/swapfile none swap defaults 0 0 | sudo tee -a /etc/fstab &&
sudo swapon -a
```

### Setup the R Mirror
When we ask Ubuntu to install applications, it grabs files from certain default 
repositories. The default R repository is usually badly out of date. We want to 
specify one of the official CRAN repositories to insure we get the most up to 
date version of R and packages.

First we make sure all the server files are up to date.
```
sudo apt-get update
sudo apt-get upgrade
```

Then, from the putty console, we're going to edit (and if needed create) an extra 
depository-specification file:
```
sudo vi /etc/apt/sources.list.d/sources.list
```

Then we want to add these lines to the file and save it. The first is for our target R
mirror (I've chosen a Seattle-based mirror). The second is for a mirror allowing Ubuntu
to get older files it may need to support the R installation (see more about this
[here](https://cran.cnr.berkeley.edu/bin/linux/ubuntu/)).
```
deb https://cran.fhcrc.org/bin/linux/ubuntu trusty/
deb http://mirrors.ocf.berkeley.edu/ubuntu/ trusty-backports main restricted universe
```

Finally, we re-run update one more time to insure the server is prepared
to work with our target CRAN mirror.
```
sudo apt-get update
```

**Notes**
* If you have any trouble using the vi editor, here's a 
    [good cheet sheet](http://www.lagmonster.org/docs/vi.html).
* The selected CRAN mirror above is a Washington state mirror. If you want to 
    use an alternate mirror, here's the 
    [official CRAN mirror list](http://cran.r-project.org/mirrors.html).
* The line points to the Ubuntu version Trusty repository. If you used a 
    different Amazon AMI than Ubuntu or if the version has changed from Trusty, 
    you will need to adjust the line accordingly.

### Install and Configure R, Shiny, and Shiny Server
Alright, it's time at last to get our core tools installed and configured. The 
steps below are described in more detail 
[here](https://github.com/chrisrzhou/RShiny-EC2Bootstrap#install-r) and 
[here](http://www.r-bloggers.com/instructions-for-installing-using-r-on-amazon-ec2/).

Then we install base R and the R development tools.
```
sudo apt-get build-dep r-base
sudo apt-get install r-base
sudo apt-get build-dep r-base-dev
sudo apt-get install r-base-dev
```

**Note**: You might get authentication warnings. Ignore these (say yes where 
needed or ignore them where they're just messages) unless you know how to 
properly setup the authentication process for R repository we're drawing our 
files from.

We give universal read/write permissions to the default R library pathway so 
that installing/managing libraries is simplified
```
sudo chmod 777 /usr/local/lib/R/site-library
```

Do another quick update request before installing the packages to make sure 
the R materials are up to date
```
sudo apt-get update
```

Now we install any R packages we need. Typically, this will just be "shiny" 
(needed before installing Shiny Server) and "packrat" (if your project is using 
"packrat" to manage its dependencies). From the console:
```
R
install.packages("shiny")
install.packages("packrat")
q()
```

And now we install Shiny Server. The way we install this has changed a few times -
when in doubt, check [here](https://www.rstudio.com/products/shiny/download-server/) 
for updated instructions (and to get the latest version number).
```
sudo apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.1.759-amd64.deb
sudo gdebi shiny-server-1.4.1.759-amd64.deb
```

We configure Shiny Server to run under the appropriate user account (ubuntu) 
and to look for our shiny apps in the right location (a subdirectory in the 
ubuntu user directory).
* Make the app-hosting folder and the logs folder (using whatever names you 
    like).
```
mkdir /home/ubuntu/shiny_apps
mkdir /home/ubuntu/shiny_logs
```
* Adjust the shiny config file to (a) point to the appropriate directories and 
    (b) run under ubuntu. More information about the config file and its 
    features is available 
    [here](http://rstudio.github.io/shiny-server/latest/#default-configuration).
```
sudo vi /etc/shiny-server/shiny-server.conf
```
* Finally, open the Shiny Server default port (3838) so that users will be able 
    to reach the hosted applications. This is done through the AWS EC2 menus 
    (not the console). Follow 
    [these directions](http://stackoverflow.com/questions/5004159/opening-port-80-ec2-amazon-web-services/10454688#10454688).

### Install and Configure Git
The best way to get our application(s) onto the EC2 server - and to setup an 
efficient development-to-production pathway - is to use git and GitHub. This 
guide assumes your applications are already hosted on GitHUb, so now you just 
need to install git.
```
sudo apt-get install git
```

**Note**: If you've never used SSH with the relevant GitHub account before, you 
might need to do 
[some more setup](https://help.github.com/articles/generating-ssh-keys/) 
before your git clone/pull requests will succeed.

### Cloning and Configuring the Shiny App(s)
At this point, we have R, Shiny, and Shiny Server installed and configured to 
play nicely with the default ubuntu account. We also have a pathway for pulling 
up to date versions of our Shiny application(s) to our server.

Now we need to clone our app(s) in our chosen app hosting folder 
(e.g., ```/home/ubuntu/shiny_apps```) and install any needed R packages or 
other resources. From here, app setup directions will be described on an 
app-by-app basis.

#### The Case Outcome Simulator (COS)
COS uses "packrat" to manage its package dependencies. All we need to do is 
clone the project and then start R in the COS application directory. "packrat"
will install all of the relevant packages from their binaries (which "packrat" 
keeps copies of). 

However, the Cairo and xml2 packages require additional material be available
on the instance. Attempts to to install these will fail unless the correct 
resources are added to our EC2 server.

I THINK the only packages missing from the default EC2 setup are these. Keep
an eye out during the "packrat" step (a little ways below) - if install
errors happen, these are often dependency related and the errors will
sometimes point you to dependencies needed.
```
sudo apt-get install libcurl4-openssl-dev
sudo apt-get install libcairo2-dev
sudo apt-get install libxml2-dev
```

Now we clone the app...
```
cd /home/ubuntu/shiny_apps/
git clone https://github.com/pocdata/COS
```

And initialize R in the COS Shiny project folder - "packrat" should 
automatically try to install all needed packages.
```
cd /home/ubuntu/shiny_apps/COS/
R
```

If "packrat" doesn't initialize:
* First make sure the working directory is correct - ```getwd()``` from the R 
    console.
* Then manually force the update (```packrat::restore()``` from the R console).

If packrat has installation errors, you'll need to problem shoot or install 
those packages manually (e.g., ```install.packages()``` from the R console).

Exception: If packrat is failing when it tries to download packages, check out 
the workaround [here](https://github.com/rstudio/packrat/issues/209).

### Test Initalize Shiny Server
At this point, it's time to initialize Shiny Server and start looking at your 
Shiny applications. This initialize will be fragile - closing your putty
session will shut down the server. We'll cover how to do a robust 
initialization in a moment - we just want to use this one for testing
purposes.

From the putty console, simply use:
```
shiny-server
```

You should see some confirmation dialogue and see that the server is running. 

**Note**: If you get EADDRINUSE errors... try changing the port Shiny is using 
(e.g., to 3939). Don't forget that you need to add this port to the EC2 inbound 
security group AND that this changes the link people need to use to reach the 
app.

### Get the Link to Your App
The link you're looking for will be a combination of (a) the EC2 server's 
Public DNS, (b) the Shiny Server port number, and (c) the application 
directory
```
[Public DNS]:[Port Number]/subdirectory
```

You can get the Public DNS using the AWS Console. Sign in to your account, go 
to the EC2 dashboard, look under "Instances" and click on your instance. 
A "Description" tab should open up with the Public DNS.

The port number will be 3838 unless you adjusted it.

The directory should be the name of your project folder, though the exact
path may vary depending on how you setup your Shiny Server and project.

Here is an example URL (the `%20` symbols are simply replacing spaces in a
sub-folder name):
```
http://ec2-52-8-38-141.us-west-1.compute.amazonaws.com:3939/COS/
```

If the app of interest is in a sub-directory (most likely), then it may be 
easiest to simply visit the base link (DNS:Port Number) and click through the 
auto-generated index to get the full link to your app.

### Creating and Managing a Robust Shiny Server Initialization
By default, running processes will close if you close your connection to 
the AWS instance (e.g., by closing putty).

There are a [few approaches to keeping a process alive](http://serverfault.com/questions/311593/keeping-a-linux-process-running-after-i-logout)
on disconnect. We'll use the `nohup` option. Adding `nohup` to our 
Shiny Server start will prevent it from being shut down (i.e., it tells the
process to ignore the `HUP` signal sent to it when the connection closes).
```
nohup shiny-server
```

This approach has a drawback, however. If you need to reset the Shiny Server -
such as when you make updates to a hosted application - then you need to do
it manually.

First you need to identify the process ID (PID) for Shiny Server. You can use
the following to get a list of all running processes:
```
ps -A
```

Look for the Shiny Server process and note its ID. Then you can kill the 
process with `kill` followed by the PID number. For example:
```
kill 10482
```

Now you're set to restart the Shiny Server as usual!

### GAME ON!!
At this point, you are either admiring your Shiny application or 
troubleshooting your way there. Congrats or best of luck, as needed!

## Getting MOS Application Updates
Unfortunately, because the COS repository is private it cannot be a standard 
fork of the MOS_framework repository. Instead, is a duplicate of that repository.

To get updates from the MOS\_framework repository, we need to manually grab 
the current MOS\_framework files (e.g., by creating a separate clone of the 
repo or downloading them via a web browser).

Files to grab: Everything *except* README.md and MOS_config.R. Those two files 
are tailored to the COS application.
