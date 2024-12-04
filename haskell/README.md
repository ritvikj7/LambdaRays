# CPSC 312 Project - MVP README

# LambdaRays

LambdaRays will be a renderer that creates realistic images using ray tracing, a technique that simulates how light behaves in the real world. By tracing rays from a virtual camera (i.e from a certain perspective), it will render simple shapes with accurate lighting, shadows, and reflections, producing high-quality, detailed graphics. The goal is to have visually appealing images.

This project is in fulfillment of the [CPSC 312 2024W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website-2024W1/project.html).

## Team Members

Our team is:

- Ritvik Joshi (72789472)
- Arman Randhawa (19407006)
- Foram Patel (44089563)

We call ourselves: **The Good, The Bad, The Ugly**.


---
## Acknowledgments

- CPSC 314 Notes
- ChatGPT
- [JuicyPixels Documentation](https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture.html#v:generateImage)
- [Dmitry Sokolov’s Understandable Ray Tracing GitHub post.](https://github.com/ssloy/tinyraytracer/wiki/Part-1:-understandable-raytracing#understandable-raytracing-in-256-lines-of-bare-c)
- [Lar Rotgers' ray-sphere intersection formula explanation](https://rotgers.io/posts/ray-sphere-intersection/)

---

## How the MVP fulfils our proposal and links to MVP code


## How to run the MVP

To run the MVP, all you need to do is run
- `stack build`
- `stack run`
- And then the output will be written out to the 'output.png'

You can also certain parameters like the image resolution, samples per pixel (spp), and number of child rays.
To adjust these parameters, you can go into [src/Camera.hs](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/arman/haskell/src/Camera.hs#L5) and adjust [spp](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/arman/haskell/src/Camera.hs#L5) to 5-15 and also adjust [imgWidth](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/arman/haskell/src/Camera.hs#L15) to be something higher resolution (like 720 or 1080). You can also adjust [rayDepth](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/arman/haskell/src/PixelShader.hs#L87) to be something higher than 3 (e.g. 5) if you want better inter-reflections, albeit I haven't really tested if it create much better results so your mileage may vary.

Note: I would not go beyond the parameters I specified given it will even take a long time at `720p` at `spp = 5` so be prepared to wait a long time if you choose to do this.

## A guide to our new learning
- Highlighting how the new learning is essential for your project/MVP
- Direct links into parts of the code where the new learning was employed
- Explanation of how the project benefits from the application of your new learning.
