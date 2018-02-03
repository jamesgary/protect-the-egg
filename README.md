# Safety Game

Play the work in progress here: http://elm-game-jam-feb-2017.s3-website-us-east-1.amazonaws.com

Protect egg!

Day 1
- [x] Draw egg (just a cirlce)
- [x] Draw hero (another cirlce)
- [x] Hero follows mouse
- [x] Draw/move enemies
- [x] Collide with hero
- [x] Collide with egg
- [x] Make hero semi-circle and rotate around egg

Day 3
- [x] Use webgl
- [x] Random enemy position upon start up

Next steps:
- [ ] Trickle of enemies (every X ms, an enemy spawns randomly around egg)
- [ ] Use the sword as a sword
- [ ] Hero slowly moves towards cursor (doesn't snap)
- [ ] Fix collision with new semi-circle hero

Future steps:
- [ ] Start Screen
- [ ] Actual Level
- [ ] Onboarding/Tutorial
- [ ] Sound
- [ ] Music
- [ ] Save progress

Possible heroes archetypes:
- Sword (long and pointy)
  - Mouse holds the hilt, rotates around egg
  - Moves fast radially, but slow inward/outward
- Shield (wide and flattish/roundish)
  - Mouse holds the handle, rotates around egg
  - Moves slow radially, but fast inward/outward
- Mace (ball + chain + stick)
  - Mouse holds the stick, swings ball around

Each hero could have 2 sides, red and blue:
- Hit red enemies with red (baddies)
- Hit blue enemies with blue (food? powerups?)

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
