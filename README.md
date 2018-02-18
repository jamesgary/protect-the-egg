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
- [x] Trickle of enemies (every X ms, an enemy spawns randomly around egg)

Day 4
- [x] Hero is a rectangle

Day 8
- [x] Restore hero/enemy collision logic

Day 9
- [x] Click to change between sword/shield/swordshield
- [x] Config
- [x] Configurable enemy speed and spawn rate get faster and faster

Day 10
- [x] Figure out WebGL and alpha

Day 11
- [x] Add particle effects

Day 13
- [x] Restore movement
- [x] Hero slowly moves towards cursor (doesn't snap)
- [x] Hero accelerates to desired location (spring!)

Day 14
- [x] Durdle power! (hero has turtlesque bod)
- [x] Clustered enemies

Day 18
- [x] Configurable cluster size
- [ ] Draw beach and ocean
- [ ] Bounce

Next steps:
- [ ] 

Future steps:
- [ ] Juice!
- [ ] Start Screen
- [ ] Actual Level
- [ ] Onboarding/Tutorial
- [ ] Sound
- [ ] Music
- [ ] Benchmark
- [ ] Quadtrees
- [ ] Save progress

### Random notes

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

---

Everytime you destroy an enemy, your "egg meter" increases
When it's full, you can lay an egg in your nest
Slightly increases size of nest, but increases final score




