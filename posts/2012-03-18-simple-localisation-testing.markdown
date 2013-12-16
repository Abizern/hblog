---
title: Simple Localisation Testing
description: You don't need to change the device settings
tags: osx, ios
---

Sometimes you want to test your localisations but you don't want to go through the hassle of
changing the settings on the simulator, or device, or your Mac for each one. There's always the fear
of setting some language that you don't understand.

But, with Xcode4 schemes and a little argument passing this is a lot easier than it used to be. You
can set up a scheme for each localisation which will let you run your iOS or Mac app under that
localisation without having to change any settings.

Just to make it easier, you can have a look at an example Xcode Workspace that I've
[put up on Github](https://github.com/Abizern/SimpleLocalisationTesting "Simple Localisation
Testing"). This has two projects within it, one for the Mac and one for the iPhone.

{% img http://images.abizern.org/2012/03/Schemes.png 631 111 Xcode Schemes %}

Start by having a look at the schemes. There are two projects with two schemes each, one for each
localisation. Handily named so you can tell which is for which.


{% img top http://images.abizern.org/2012/03/RunAction_en.png 525 135 en run action %}

{% img top http://images.abizern.org/2012/03/RunAction_es.png 525 133 en run action %}

If you have a a look at the run action for these schemes you can see the arguments that I am
passing. First the `-AppleLanguages` argument and then the two character country code (with the
brackets back-slash escaped).

That's all there is to it. Now you just have to pick the scheme to run and it will load up that
localisation without you having to change it on the simulator. It even works for running debug
builds on the device.

Of course, you should test it properly eventually, but while you are developing, it's a quick way to
get feedback about how your localisations appear.
