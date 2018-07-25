# Yesod-notes
This is just a small experiment. It's a note-taking application that I made to get familiar with the [Yesod Framework](https://www.yesodweb.com).

You can visit the website online, it's [hosted on Heroku here](https://yesod-notes.herokuapp.com/).

Yesod has a lot of interesting features, this website also makes use of:

 - [Bulma CSS Framework](https://bulma.io/), because I wanted to try it. Easy to use, it only took a little to make [another Form Renderer](https://github.com/pasqu4le/yesod-notes/blob/master/src/Form/Bulma.hs).
 - [Unobtrusive JavaScript](https://en.wikipedia.org/wiki/Unobtrusive_JavaScript), because I like to avoid JavaScript when I can. I am pleased to say that this website can work without it, but I had to use [a little CSS trick](https://www.inserthtml.com/2012/04/css-click-states/) to make the navbar dropdown menu work on mobile.
 - [Session Cookie Explanation](https://yesod-notes.herokuapp.com/cookies), so for every collected info one can know if it's used and what for.

## JSON API
Most pages can receive/return json. You just have to set `application/json` as your HTTP Accept Header.

> Note: you will also need to keep and send the session cookie if you try to access a page that requires authentication.

I will document this as soon as I can, but feel free to try it.

## Suggestions and critiques are always welcome
If you want to request a feature, point out a bug or discuss about something: [open up an issue on github](https://github.com/pasqu4le/yesod-notes/issues/new).
