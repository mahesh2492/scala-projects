package com.learning.photoscala

import java.awt.image.BufferedImage
import java.io.{File, FileInputStream}
import javax.imageio.ImageIO

class Image private (private val bufferedImage: BufferedImage){
   val width = bufferedImage.getWidth
   val height = bufferedImage.getHeight

   def getColor(x: Int, y: Int): Pixel =
      Pixel.fromHex(bufferedImage.getRGB(x, y))

   def setColor(x: Int, y: Int, p: Pixel): Unit =
      bufferedImage.setRGB(x, y, p.toInt)

   def save(path: String) =
      ImageIO.write(bufferedImage, "JPG", new File(path))

   def saveResources(path: String): Unit =
      save(s"photoscala/src/main/resources/$path")

   def crop(startX: Int, startY: Int, w: Int, h: Int): Image = {
      assert(
         startX >= 0 &&
           startY >= 0 &&
           w > 0 && h > 0 &&
           startX + w < width && startY + h < height
      )

      val newPixels = Array.fill(w * h)(0)
      bufferedImage.getRGB(startX, startY, w, h, newPixels, 0, w)
      val newBufferedImage = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB)
      newBufferedImage.setRGB(0, 0, w, h, newPixels, 0, w)
      new Image(newBufferedImage)
   }
}

object Image {
   def load(path: String): Image = {
      new Image(ImageIO.read(new File(path)))
   }

   def loadResource(path: String): Image =
      load(s"photoscala/src/main/resources/pixels/$path")

   def main(args: Array[String]): Unit = {
      loadResource("nebula.jpg").crop(200, 100, 500, 600)
        .saveResources("nebula_cropped.jpg")
   }
}