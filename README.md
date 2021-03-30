# SAFE Template
This template can be used to generate a full-stack web application using the [SAFE Stack](https://safe-stack.github.io/). It was created using the dotnet [SAFE Template](https://safe-stack.github.io/docs/template-overview/). If you want to learn more about the template why not start with the [quick start](https://safe-stack.github.io/docs/quickstart/) guide?

## Install pre-requisites
You'll need to install the following pre-requisites in order to build SAFE applications

* The [.NET Core SDK](https://www.microsoft.com/net/download) 3.1 or higher.
* [npm](https://nodejs.org/en/download/) package manager.
* [Node LTS](https://nodejs.org/en/download/).

## Starting the application
Before you run the project **for the first time only** you must install dotnet "local tools" with this command:

```bash
dotnet tool restore
```

To concurrently run the server and the client components in watch mode use the following command:

```bash
dotnet fake build -t run
```

Then open `http://localhost:8080` in your browser.

To run concurrently server and client tests in watch mode (run in a new terminal):

```bash
dotnet fake build -t runtests
```

Client tests are available under `http://localhost:8081` in your browser and server tests are running in watch mode in console.

## SAFE Stack Documentation
If you want to know more about the full Azure Stack and all of it's components (including Azure) visit the official [SAFE documentation](https://safe-stack.github.io/docs/).

You will find more documentation about the used F# components at the following places:

* [Saturn](https://saturnframework.org/docs/)
* [Fable](https://fable.io/docs/)
* [Elmish](https://elmish.github.io/elmish/)

## About
This website was written using the SAFE stack template as described above in the pregenerated above. Using that template as a boiler plate, it has been turned into a
website that I am currently using to showcase some technical, design and creative abilities as they currently stand. I am fairly new to functional programming in general
but find it highly inteeresting, rewarding and fun. This is currently a work in progress, and will have better documentation in a future commit, I promise :)

This is not in a completed state and is currently a WIP. There is still quite a lot left that I still need to do, like implement some of the games like pivot points & 
tile smash, add other apps and language script examples, finish up styling and then correctly override them through Fulma, add more pages for the about section to 
expand those cards and give full details, put in meaningful details, and add comment and clean up other comments, etc, etc....


- Currently deployed as an azure website for testing purposes, which can be found at: seanwilken.azurewebsites.net
- There is currently no server routing, so the extra path's after the above URL won't be server reachable. 
- Reload the base link and renavigate to your desired section of the site.