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
To adjust these parameters, you can go into [src/Camera.hs](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/d31e67dc95db46f7276bd3723dc53987f3728ff9/haskell/src/Camera.hs#L6) and adjust [spp](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/d31e67dc95db46f7276bd3723dc53987f3728ff9/haskell/src/Camera.hs#L6) to 5-15 and also adjust [imgWidth](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/d31e67dc95db46f7276bd3723dc53987f3728ff9/haskell/src/Camera.hs#L15) to be something higher resolution (like 720 or 1080). You can also adjust [rayDepth](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/d31e67dc95db46f7276bd3723dc53987f3728ff9/haskell/src/PixelShader.hs#L88) to be something higher than 3 (e.g. 5) if you want better inter-reflections, albeit I haven't really tested if it create much better results so your mileage may vary.

Note: I would not go beyond the parameters I specified given it will even take a long time at `720p` at `spp = 5` so be prepared to wait a long time if you choose to do this.

## A Guide to complete MVP implementation in detail


Let us go through each file and explain what each chunk of code is doing:

First of all, let us talk about the [Vec3.hs](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/main/haskell/src/Vec3.hs) file:
This module is implementing to initialize the vector in 3 dimensions that can be used to present the point on the image, point the direction of another vector, also can be used to store the value of the color in form of RGB to print the corresponding color of each pixels which is used extensively to produce the desired image after interacting with the objects, light source and camera lens.
Following is the smaller chunk of code and a little explanation on what it is doing:
1.	https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/Vec3.hs#L12-L32
    This chunk of code is implementing the instance of the Vec3 in Num and supports the basic arithmetic operations such as addition, subtractions, negation, absolute and normalization of the vectors in 3 dimensions.
2.	https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/Vec3.hs#L35-L72
    This chunk of code is implementing few additional operations that can be used to operate on vectors that are extensively used later on during calculation of the ray determinant to check if the ray is interacting with the object or not. By implementing operations such as dot and cross product along with few additional implementations such as calculation of the vector length and few others.

Thus, this is the guide on Vec3.hs file and what it is implementing.

Let’s move on and talk about the [Camera.hs](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/main/haskell/src/Camera.hs) file:
This module is implementing to initialize the camera position along with setting the size of the width and height of the image that will be generated. Moreover, this module also sets few other constraints that are extremely important to generate the image such as setting the spp also known as samples per pixels, which states that how many samples we need to have per pixel to generate the RGB component for that particular pixel. In our implementation we are assigning samples as the number of rays to be inserted/emitted per pixels to generate the corresponding correct RGB values for a pixel. In addition, this file also works on creating rays from the camera lens to the co-ordinates passed.
Following is the smaller chunk of code and a little explanation on what it is doing:
1.	https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/c7e68e37286e71f3287a0a4a6ffa7c4bad6d6970/haskell/src/Camera.hs#L5-L21
    This chunk of code is setting the aspect ratio of the image and setting the data of the width and height accordingly to the width passed and aspect ratio mentioned.
2.	https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/c7e68e37286e71f3287a0a4a6ffa7c4bad6d6970/haskell/src/Camera.hs#L24-L34
    This chunk of code is Defining the width and height of the image in world coordinates as vectors and normalized versions (imgVecX and imgVecY) represent unit vectors in the x and y directions.
3.	https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/c7e68e37286e71f3287a0a4a6ffa7c4bad6d6970/haskell/src/Camera.hs#L36-L52
    This chunk of code setting the viewport for the camera along with coverting it into the vectors and also calculating its size in one pixel in x (width) and y (height).
4.	https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/c7e68e37286e71f3287a0a4a6ffa7c4bad6d6970/haskell/src/Camera.hs#L54-L73
    This is actually placing the virtual camera in the setting alongside setting its focal length and the plane near the camera. Moreover, this chunk of code also defines the co-ordinates that are going to be used by other part of the code extensively to calculate the vector direction from the camera to the co-ordinates the program is working on that a particular point in time.
5.	https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/c7e68e37286e71f3287a0a4a6ffa7c4bad6d6970/haskell/src/Camera.hs#L78-L86
    This part of the code works on generating the rays and also calculating the point in the setting that the ray can reach given its point and direction.

Thus, this is the guide on Camera.hs file and what it is implementing.


## A guide to our new learning

One of the new learning areas that we learned during implementation of the MVP is using JuicyPixels library that helps in writing and generating the new images using the RGB list vectors. On to why this is an essential part of the project/MVP goes to the promises that we committed to implement in the MVP, which states that we are going to convert the list of vectors we get after calculating the interaction with all the objects, lights and camera with a particular ray and present the final output as a image rather than showing it in the text form in the terminal. Thus, here comes the JuicyPixels library which helps in converting the list of RGB vectors that each represent the pixel in an image and generate the corresponding image using the GenreateImage and also the WritePng functions and can also use many other in-built functions that supports multiple different files. For our implementation, we are using the JuicyPixels library’s built-in functions such as writePng and generateImage functions to generate the image as the png file type.
https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/d3cb9a0ba16b048021c4762b6797f0bb7108099d/haskell/src/Utils.hs#L31-L34
Talking about the benefits this new learning area has brought to the project/MVP is that is has made it easier for us to see the generated image which is much better than the text based image generation it has that is hard to see and also does not support the colors. Thus, using JuicyPixels has made it easier and available for us to show the image in more meaningful way that is understandable for the whole world and explains on what we need in real world as well.

