# Safety Game

Play the work in progress here: http://elm-game-jam-feb-2017.s3-website-us-east-1.amazonaws.com

Protect egg!

First steps...
- [x] Draw egg (just a cirlce)
- [x] Draw hero (another cirlce)
- [x] Hero follows mouse
- [x] Draw/move enemies
- [x] Collide with hero
- [x] Collide with egg
- [x] Make hero semi-circle and rotate around egg

Next steps:
- [ ] Fix collision with new semi-circle hero
- [ ] Random enemy position upon start up
- [ ] Trickle of enemies
- [ ] Hero slowly moves towards cursor (doesn't snap)

Iterate!

Various shapes, various movement/rotation

- Line/Block : Always static rot
- Cup : Rotates around egg
- Any : Follows mouse rot, Moves to mouse when mousedown relative (or up?)
  - May want to lock mouse (html5 api)

There should be some dissencentive for moving mouse/shield over egg
- slows down shield?
- damages egg?

any way to move physics / game logic into GPU?
