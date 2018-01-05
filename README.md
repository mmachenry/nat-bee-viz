# nat-bee-viz
A data visualization for the life time of worker bees.

You can see it in action at https://mmachenry.github.io/nat-bee-viz/

To run this locally and test changes, use this command in the main directory.

    elm-reactor

To build a new html file for deployment, use this command in the main directory. Be sure to add the docs/index.html file to a checkin if you want it to be updated on the the github.io site mentioned above.

    elm-make src/Main.elm --output docs/index.html
