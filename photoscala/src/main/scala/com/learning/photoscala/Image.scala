package com.learning.photoscala

import java.awt.image.BufferedImage
import java.io.{File, FileInputStream}
import javax.imageio.ImageIO

class Image private (private val bufferedImage: BufferedImage){
   val width: Int = bufferedImage.getWidth
   val height: Int = bufferedImage.getHeight

   def getColor(x: Int, y: Int): Pixel =
      Pixel.fromHex(bufferedImage.getRGB(x, y))

   def setColor(x: Int, y: Int, p: Pixel): Unit =
      bufferedImage.setRGB(x, y, p.toInt)

   private def save(path: String) =
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

   def map(f: Pixel => Pixel): Image = {
      val newPixels = Array.fill(width * height)(0)
      bufferedImage.getRGB(0, 0, width, height, newPixels, 0, width)
      newPixels.mapInPlace { color =>
        val pixel = Pixel.fromHex(color)
        val newPixel = f(pixel)
        newPixel.toInt
      }
      val newBufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      newBufferedImage.setRGB(0, 0, width, height, newPixels, 0, width)
      new Image(newBufferedImage)
   }

   //case class Window(width: Int, height: Int, pixels: List[Pixel])
   def window(x: Int, y: Int, width: Int, height: Int): Window = {
      val offsetX = (width - 1) / 2
      val offsetY = (height - 1) / 2
      val horizontalCords = ((x - offsetX) to (x + offsetX))
        .map { v =>
          if(v < 0) 0
          else if(v >= this.width) this.width - 1
          else v
        }
      val verticalCords = ((y - offsetY) to (y + offsetY))
        .map { v =>
           if(v < 0) 0
           else if(v >= this.height) this.height - 1
           else v
        }

      val pixels = for {
         xp <- horizontalCords
         yp <- verticalCords
      } yield getColor(xp, yp)

      Window(width, height, pixels.toList)
   }
}

object Image {
   def apply(width: Int, height: Int, pixels: Array[Pixel]): Image = {
      val newBufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      newBufferedImage.setRGB(0, 0, width, height, pixels.map(_.toInt), 0, width)
      new Image(newBufferedImage)
   }
   def black(width: Int, height: Int): Image = {
      val color = Array.fill(width * height)(0)
      val bufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      bufferedImage.setRGB(0, 0, width, height, color, 0, width)
      new Image(bufferedImage)
   }
   def load(path: String): Image = {
      new Image(ImageIO.read(new File(path)))
   }

   def loadResource(path: String): Image =
      load(s"photoscala/src/main/resources/$path")

   def main(args: Array[String]): Unit = {
      loadResource("nebula.jpg").crop(200, 100, 500, 600)
        .saveResources("nebula_cropped.jpg")

      loadResource("blue_marble.jpg").crop(200, 100, 500, 600)
        .saveResources("blue_marble_cropped.jpg")

      loadResource("metal_worn.jpg").crop(0, 0, 500, 600)
        .saveResources("metal_worn_cropped.jpg")

   }
}