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
- [x] Draw beach and ocean
- [x] Bounce

Day 22
- [x] Bump the hero when colliding

Day 25
- [x] Sidebar (basic)
- [x] Enemy sequencing

Day 26
- [x] Art!
- [x] Sound!

Day 27
- [x] Durdle goes around nest
- [x] Durdle stays on the beach
- [x] Increase kaiju after quab hit
- [x] Kaiju progress bar
- [x] Quabs slowly munch on eggs
- [x] Start Screen
- [ ] Onboarding/Tutorial
- [ ] Victory
- [ ] Music
- [ ] Better game over (restart)

Future steps:
- [ ] C-C-C-COMBOS
- [ ] Juice!
- [ ] Actual Level
- [ ] Benchmark
- [ ] Quadtrees
- [ ] Save progress

### Credits

- Sounds: https://www.bfxr.net
- Textures: https://transparenttextures.com
- Audio API: https://howlerjs.com
- Sprite Maker: http://draeton.github.io/stitches

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

---

Tablet stuff:
- Draw start screen
- Draw durdle
  - Durdle eyes?
  - Durdle waddling hands?
- Draw quab
  - Quab walk (DONE)
  - Quab eating egg
  - Quab exploding
- Draw bg w/ egg nest (DONE)
- Draw an egg in these trying times
  - Egg getting eaten
- Victory screen
  - Eggs hatch
  - Baby durdles express gratitude
- Failure screen (meet someone nice, lay eggs, and try again?)

Computer stuff:
- Start screen button
- Sidebar
  - Pause/options
  - Time until hatch
  - Score
  - num eggs left
  - Kaiju meter
- Onboarding (?)
- Music (surf rock from FMA)
- Sound
  - Start game
  - Crab greeting
  - Crab death
  - Durdle tummy hit
  - Crab ate egg
  - Pirate greeting
  - Pirate shooting (aim, fire)
  - Pirate death
  - Game over
  - Victory
- Sequence enemies
- Pirates
  - Stands still, laser pointer aim
  - Later: Rotates around island
- Eye tracking (follows cursor, but don't go cross-eyed)

Options:
- Music
- Sound
- Graphics quality(?)
- Always follow mouse or Right click to move

Still to decide
- How to onboard? Random dialogue boxes? Or Shelly Durdle speaking? From cursor, or aside?

Maybe level progression:
- Just crabs, different colors, intro to kaiju, bg is all sand
- Zoom out, see shore, introduce pirates

Quabs are chomping at the bit for your eggs!
Smash them away with your shell!
The more quabs you smash, the more your Kaiju meter increases!
Click to unleash your Kaiju rage!
