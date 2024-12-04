# CPSC 312 Project - Minimum Viable Product

# LambdaRays

LambdaRays is a renderer that creates realistic images using ray tracing, a technique that simulates how light behaves in the real world. By tracing rays from a virtual camera (i.e from a certain perspective), it renders simple shapes with accurate lighting, shadows, and reflections, producing high-quality, detailed graphics. It's goal is to have visually appealing images.

This project is in fulfillment of the [CPSC 312 2024W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website-2024W1/project.html).

## Team Members

Our team is:

- Ritvik Joshi (72789472)
- Arman Randhawa (19407006)
- Foram Patel (44089563)

We call ourselves: **The Good, The Bad, The Ugly**.

## Demo Video
- Here is the link to the demo video: https://youtu.be/Yvb3GtVKB5Y?si=NFWiHo3Mr2g9wP0j

## Acknowledgments

- CPSC 314 Notes
- ChatGPT
- [JuicyPixels Documentation](https://hackage.haskell.org/package/JuicyPixels-3.3.9/docs/Codec-Picture.html#v:generateImage)
- [Dmitry Sokolov’s Understandable Ray Tracing GitHub post.](https://github.com/ssloy/tinyraytracer/wiki/Part-1:-understandable-raytracing#understandable-raytracing-in-256-lines-of-bare-c)
- [Lar Rotgers' ray-sphere intersection formula explanation](https://rotgers.io/posts/ray-sphere-intersection/)

---

## How the MVP fulfils our proposal and links to MVP code?

Our MVP initially outlined fundamental features typical of a basic Whitted-style ray tracer. These features are intentionally simplified compared to those found in production-grade renderers, lacking advanced effects such as soft shadows, post-processing (bloom, tone mapping, depth-of-field), industry-standard BSDF models like GGX and the Disney BSDF, and comprehensive global illumination.

While our current implementation doesn't include these high-end capabilities, we've established a solid foundation that can be expanded upon. For instance, with our existing shadow ray shading functionality, we could expand it to incorporate soft shadows using distributed ray tracing or filtering techniques like PCSS.

- Starting with our 1st feature, we outlined that we wanted **“Image Support”** so we could render high-quality images instead of using an ASCII terminal as our framebuffer which would severely limit the fidelity of our renders. We used the `JuicyPixel` library to facilitate marshalling our `Vec3` pixel frame buffer to `PixelRGB8` data that we can write into a PNG file.
Here is the link to the image support code: https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/Utils.hs#L28-L34
- For our 2nd feature, we outlined that we wanted **“Scene Geometry Primitives Support”**. What that entails, is that we can populate the world with objects other than just a sky background, and we did that using a Sphere. Spheres have a very nice closed-form analytical formula that we can query for intersections such that we can render them in our scene. Here is the code that represents that models the `SphereRecord` ADT as an instance of the `SceneObject` ADT and code that solves the intersection between a ray and a sphere: https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/ScenePrimitives.hs#L11-L50
- For our 3rd feature, we outlined that we wanted **”Basic Shading Support”**. We have essentially accomplished this by adding a very well-established BRDF called Blinn-Phong for applying specular, Lambertian Reflectance for applying diffuse, and simple ambient colour to our shading of non-metallic (matte) surfaces. While not as physically-based as something like GGX, it is a very strong and effective shading model that accomplishes our needs for the MVP. Here is the link to the code: https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/PixelShader.hs#L62-L76
- For our last feature, we outlined that we wanted **”Primitive shadow ray support”**. We have accomplished that by adding a `shadowRay` query into our `shadeSphere` function. We check for intersections from this `shadowRay` to every other scene object (excluding itself to avoid self-intersection to avoid shadow acne) to see if the pixel is occluded from the light. If it is, we shade it the ambient colour, otherwise, we shade it as if it were exposed to light. Here is the link to the code: https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/PixelShader.hs#L33-L45

### How to run the MVP:

To run the MVP, all you need to do is run
- `stack build`
- `stack run`
- And then the output will be written out to the 'output.png'

You can also control certain parameters like the image resolution, samples per pixel (spp), and number of child rays.

To adjust these parameters, you can go into [src/Camera.hs](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/d31e67dc95db46f7276bd3723dc53987f3728ff9/haskell/src/Camera.hs#L6) and adjust [spp (samples per pixles)](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/d31e67dc95db46f7276bd3723dc53987f3728ff9/haskell/src/Camera.hs#L6) to 5-15 and also adjust [imgWidth](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/d31e67dc95db46f7276bd3723dc53987f3728ff9/haskell/src/Camera.hs#L15) to be something higher resolution (like 720 or 1080). You can also adjust [rayDepth](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/d31e67dc95db46f7276bd3723dc53987f3728ff9/haskell/src/PixelShader.hs#L88) to be something higher than 3 (e.g. 5) if you want better inter-reflections, albeit we haven't really tested if it create much better results so your mileage may vary.

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

Here is the code for the [PixelShader.hs](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/PixelShader.hs) file:
This module is doing a job akin to a fragment shader in a typical GPU-driven renderer with the caveat that we are doing recursive sampling. For a given pixel in the framebuffer, we take `spp` samples offset by some random amount that is eventually averaged out to find the final colour. Each sample has a ray sent toward the pixel and queries whether it intersects any spheres and shades the colour based on that.
1.  https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/PixelShader.hs#L30-L31
	  This chunk of code draws the background gradient for areas not intersected by objects. This creates a gradient from blue (sky) to white based on the y-coordinate of the ray
2. https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/PixelShader.hs#L47-L60
	  This chunk of code shades a reflective sphere by tracing the reflected ray into the environment
3. https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/PixelShader.hs#L78-L90
	  This chunk of code takes `spp` samples within the pixel offset by `(offsetX, offsetY)` (random at every sample), and decides whether to shade based on if it intersects a sphere or the sky recursively per sample.

Thus, this is the guide on PixelShader.hs file and what it's implementing

Here is the code for the [ScenePrimitives.hs](https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/ScenePrimitives.hs) file:
This module responsible for finding whther any of our rays intersect with any of objects in our scene. It includes functions to determine whether a ray intersects a sphere and at what parameter `t` is hits it, and also does visibility determination by finding the object that's nearest to the camera
1.  https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/ScenePrimitives.hs#L36-L50
	  This chunk of code computes the closed-form intersection formula for a Ray-Sphere intersection and finds the `t` value that scales the ray direction vector enough to hit the sphere
2. https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/ScenePrimitives.hs#L60-L65
	  This chunk of code does visibility determination by finding the closest sphere (which ray that hit the sphere has the smallest `t` value) and returns that sphere as the closest. If the ray didn't intersect any sphere at all, we just return an erroneous result.



## A guide to our new learning

- One of the new learning areas that we learned during implementation of the MVP is using JuicyPixels library that helps in writing and generating the new images using the RGB list vectors. On to why this is an essential part of the project/MVP goes to the promises that we committed to implement in the MVP, which states that we are going to convert the list of vectors we get after calculating the interaction with all the objects, lights and camera with a particular ray and present the final output as a image rather than showing it in the text form in the terminal. Thus, here comes the JuicyPixels library which helps in converting the list of RGB vectors that each represent the pixel in an image and generate the corresponding image using the GenreateImage and also the WritePng functions and can also use many other in-built functions that supports multiple different files. For our implementation, we are using the JuicyPixels library’s built-in functions such as writePng and generateImage functions to generate the image as the png file type. https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/d3cb9a0ba16b048021c4762b6797f0bb7108099d/haskell/src/Utils.hs#L31-L34
  - Talking about the benefits this new learning area has brought to the project/MVP is that is has made it easier for us to see the generated image which is much better than the text based image generation it has that is hard to see and also does not support the colors. Thus, using JuicyPixels has made it easier and available for us to show the image in more meaningful way that is understandable for the whole world and explains on what we need in real world as well.


- One of the other new learning areas that we learned during the implementation of the MVP is using the formula to calculate if the directed ray is interacting with the sphere or not. By using the formula that calculates the dot product with itself (ray direction) along with few additional calculations such as b that calculates the dot product between the ray direction and a vector that is the difference between ray origin and the position on the sphere. We use these formulas to calculate whether there is a determinant that conveys that the ray directed towards the mentioned position on the sphere is actually intersecting with it and would be diverted to different direction. On to how it is an essential part for the project/MVP. We know that rays are always directed into the one single direction until and unless it gets stuck by the object and has to get reflected in other directions. Since, we are generating the image using the rays, we know that we can only detect the object if the ray is directed to a different direction from the initial directions. And this function helps in detecting if that ray is interacting with the sphere or not by calculating the determinant and if the determinant is positive is means it is interacting with the object, in our case it is sphere. Which will not make the new function calls to get the corresponding pixel color to print in the image.https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/6e3f4d7a001cdf88bcab0f77bc52bfe0d2c7b601/haskell/src/ScenePrimitives.hs#L38-L48
  - With the help of this function, we achieved the learning area of learning on how to calculate if the ray directed is interacting with the sphere or not. Since, with this method we can easily calculate if a ray directed is interacting with a particular object in the image or not, which now we can use it to actually generate the color it will produce and also the point in the image where we will see the shadow and the reflection as well if the material of the object is reflective.


- Another aspect that was fundamental to get right for our raytracer's MVP implementation was how the shadows looked and were dispatched. Shadows are very important to get right to have a sense of placement and depth in a world, however, a buggy implementation can wipe those benefits away completely and can even worsen the presentation. When we first implemented shadow rays, we were getting very bad shadow acne issues due to the shadow rays self-intersecting with the sphere it is shot out from, leading to very random black spots on the spheres. This was very tough to debug because it's not a bug in the typical sense (as in faulty logic) but instead an artifact of floating point precision. So it was hard to track down, but after a while, we managed to figure it out by turning off diffuse shading and to pinpoint when exactly was the shadowing going wrong without the noise of diffuse also adding some darkening. We fixed it by filtering out the current sphere to avoid self-intersections: https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/PixelShader.hs#L35
  - Adding this change completely removed shadow acne, fixing the ugly look of the old shadows that were detrimental to the presentation. It saved the **"Primitive Shadow Ray Support"** for the MVP and was quite a big learning lesson that we'll look out for in new renderers.


- The last aspect that we'll touch on that was a huge enhancement to our raytracer MVP implementation was adding reflections. While we never posed it as a required feature due to it being on the more complex side to get right, we really wanted to add reflections to enhance the presentation of the output image and exploit the capability to get accurate off-screen reflections. Especially, since reflections are extremely challenging to do in other traditional rendering methods like rasterization. Therefore, we added code for metallic materials that we currently treat as perfect mirrors (didn't get to creating fuzzy/blurry reflections) here: https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/ScenePrimitives.hs#L80-L82. And then we actually add reflection code here: https://github.students.cs.ubc.ca/xyzarman/CPSC312-Haskell-Project/blob/dbe3946e72d6ba6edada83e0806f6ceb2e867e4d/haskell/src/PixelShader.hs#L47-L60
   - This is a recursive algorithm that traces reflection rays through the scene, accumulating the contributions of each bounce ray along the way, leading to the final reflective look on the spheres (There's even support for inter-reflections (albeit very primitive))! It greatly benefited the look of the renderer as we get cool inter-reflections and off-screen reflections as mentioned earlier. This was a cool learning opportunity because it was a more challenging feature to think about mathematically and also to implement in Haskell using mutual recursion (thanks 110!).

