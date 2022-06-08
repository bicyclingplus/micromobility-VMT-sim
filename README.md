# North American Micromobility Project (NAMP)

Contained here are the elements of the NAMP project, intended to estimate the extent to which micromobility use causes car use to decline.

The project is run by Dillon Fitch

Data from Dillon's earlier JUMP bike survey are in `data/jump_sacramento_data`. This is where I am getting my estimates of how many JUMP trips a person takes, how long those trips are, and whether they substitute for car travel.

# Overview
The model is to estimate the marginal change in VMT with each bike share mile traveled. Here are the components of that model:

 - [household VMT] is the distribution of weekly VMT by a household. It is estimated from the Puget Sound data, where individual cars were tracked over a year. There are other vehicle surveys but they all seem to track individual cars for about three days.
 - [user-level bikeshare miles | trip made] is the distribution of bike share miles per trip, by user_id. This comes from the Sacramento JUMP bike survey, where riders were asked how about their three most recent bike share trips, as well as some other surveys (I think).
 - [bikeshare frequency | user_id] Distribution of how many times per week user_id uses bikeshare. The data come from the Sacramento JUMP bike survey.
 - [mode substitution | bikeshare, distance] This is the probability that a bikeshare ride substituted for driving, conditional on the fact that bikeshare was chosen, and the distance. This is estimated from a large number of bikeshare surveys that all ask about mode substitution. The surveys were compiled by Dillon and his lab, and his lab also created a model that relates mode substitution proportion to the distance traveled.

## Combining the pieces
We want the distribution of the change in miles driven with a marginal increase in bike share miles. That distribution might be written as [delta(VMT) | add one additional bikeshare mile].

This distribution is unlike the components that go in to the model in these ways:

1. It is a distribution over a population, rather than linked to a single user_id
2. It is a difference involving a counterfactual (a trip is either made by bikeshare or not, and this model expects us to know what would happen in the counterfactual case).
3. It assumes that the VMT and the bikeshare miles come from the same people, but that's not how the data was collected.
4. It is a model of bikeshare miles, not reported bikeshare miles. Since the bikeshare data was collected by asking users about their usage, the data represents reported bikeshare mileage.
5. It is a model of mode substitution, not reported mode substitution (same issue as the previous point).
6. It is marginal to location, while the VMT and bikeshare data were collected in specific locations, which were generally not the same as each other.
7. The bikeshare miles and trip count were estimated by asking users who were intercepted hile using bike share, and these users might not be representative of the people who will be found by the survey.

In order to proceed, we have to address these issues. Here are responses:

1. The datasets for VMT and bikeshare contain enough different users (403 bikeshare users in the JUMP survey and 328 households in the Puget Sound driver study) that we can characterize the population of drivers/bikeshare users.
2. We are forced to make an assumption here, and we have chosen to assume that if a bikeshare trip was reported as substituting for car travel, then the number of miles of that bike trip were subtratcted from what would have been the counterfactual VMT.
3. Here another assumption is necessary. We choose to believe that the distribution [household VMT | not bikeshare user] is equal to [household VMT | bikeshare user]; [household VMT | Puget Sound] is equal to [household VMT | Sacramento]; and [bikeshare miles, bikeshare trips | Sacramento] is equal to [bikeshare miles, bikeshare trips | Puget Sound]. Obviously, the idea that the distributon of household VMT is identical between bikeshare users and nonusers would negate negate the notion of a difference in VMT due to bikeshare use. However, the actual difference is probably small enough that this assumption is safe. The assumptions about equivalent VMT and bikeshare use between Sacramento and Puget Sound seems difficult to support.
4. Here it is necessary to assume that the reported bikeshare miles are accurate.
5. Similarly, it is necessary to assume that the reported number of bikeshare trips is accurate.
6. Partly, this was covered in (3). However it is also possible that other studies can be used to estimate VMT, bikeshare mileage and mode substitution. There are a lot of transportation studies that collect VMT data from households for about three days. These might be adapted to use in a model of weekly VMT. There are many studies from different cities that ask about mode subtitution of bikeshare trips. There might also be some that ask about bikeshare mileage.
7. Here it is necessary to assume that the population who was reached in the Sacramento JUMP bike study is reflective of the population of Sacramento bikeshare users. Actually, the Sacramento JUMP bike survey might have also included a survey element as well as the random intercepts of users.


### OK, so really: combining the pieces:
Remember that the goal is [delta(VMT) | add one additional bikeshare mile]. In order to estimate this quantity using linear model techniques, it is necessary to have draws from the joint distribution of VMT and bikeshare miles ([VMT, bikeshare miles] ). Getting draws from this joint distribution is atually fairly simple, since the way we are compiling data from multiple sources means that we are assuming that bikeshare miles and VMT* are independent.

Here, I use VMT* to indicate the VMT prior to subtracting out the miles that were replaced by bikeshare miles. Also note that it would be possible to simulate VMT* and bikeshare miles in a way that is not independent. For example, the two could be taken as marginal distributions that are then convolved with a Gaussian copula. That's not what I'm doing here now, though.

This is not a traffic-generation model, so [ bikeshare miles ] is sampled directly (i.e., we don't say that person A is going to take a trip of 4 miles, then calculate how likely it is to be taken by bikeshare.) The process is kind of "backward": we sample from the distribution of how many bikeshare trips person A takes in a week, and that is how many samples to draw from the distribution of what is the distance of a bikeshare trip for person A. Add these up and you get the total bikeshare distance that A travelled in the week. For each bikeshare trip, use its distance in a model for how likely did it substitute for car travel. (E.g, if A took a 2.5-mile ride, we may suspect that their alternative was to drive their car. If our sample draws say that A took a 0.2 mile bikeshare trip, then it probably would have been a walking trip otherwise.)

So, repeated samples like [ person-parameters ]; [ # of bikeshare trips | person-parameters ]; [ length of bikeshare trips | person-parameters ]; [ mode-substitution probability | length of bikeshare trips, person-parameters ];
is how we generate samples from the joint distribution [ # of bikeshare trips, mode-substitution, length of bikeshare trips, person-parameters ]. We can marginalize over person-parameters and # of bikeshare trips simply by ignoring those (they are nuisance levels in the hierarchy of getting to the joint of total trip distance and VMT offset distance)

total bikeshare distance | person-ID = sum( length of bikeshare trips | person-ID )
VMT offset = sum( bikeshare trip distance x does trip substitute for car travel? | person-ID )

and,

[ does trip substitute for car travel? ] = Bernoulli( car substitution probability )


Then VMT* comes from [ person-parameters ]; [ VMT* | person-parameters ]. Combined, these draws give the joint distribution [ VMT*, person-parameters ] which is marginalized over person-parameters by just ignoring that variable. 

Finally, VMT is calculated by VMT = VMT* - VMT offset.

Now we have simulated the necessary data: the joint distribution [VMT, bikeshare miles].

# Simulation

In order to simulate hypothetical scenarios, I need to set up a reasonable simulation framework. to that end, I have these scripts:

## `explore-sim.R`
creates a model for the weekly vehicle miles traveled, assuming cars are independent of each other. There is currently a major flaw in the model, which is apparent because there is almost perfect correlation in the draws of the gamma parameters.


## `simulate.R`
 - Generates simulated samples of bike miles traveled and vehicle miles traveled for 2000 survey respondents.
 - Bike share frequency is given over a 28 day period. Crude: assume Poisson, divide mean by four and use as the intensity. Just a simple mean (10.9 trips) - variance (213.3 trips^2) comparison shows that the Poisson distribution isn't appropriate here, so I will have to build in overdispersion. Later, though.
 - OK, I have modeled the number of bikeshare trips as negative binomial, with parameters estimated from the marginal distribution of trips. Now the overdispersion is too great.
 - moved division inside the variance calculation for the negative-binomial, and the draws look more realistic now but I think this is all too ad hoc and must be approached in a more thoughtful way.
 
