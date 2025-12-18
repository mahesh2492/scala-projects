package com.learning.photoscala

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

case class Pixel(red: Int, green: Int, blue: Int) {
  assert(red >= 0 && red < 256 && green >= 0 && green < 256 && blue >= 0 && blue < 256)

  def toInt: Int =
    (red << 16) | (green << 8) | blue

  infix def +(other: Pixel): Pixel =
    Pixel(
      Pixel.clamp(red + other.red),
      Pixel.clamp(green + other.green),
      Pixel.clamp(blue + other.blue)
    )


  def draw(width: Int, height: Int, path: String): Boolean = {
    val color = toInt
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val pixels = Array.fill(width * height)(color)
    image.setRGB(0, 0, width, height, pixels, 0, width)
    ImageIO.write(image, "JPG", new File(path))
  }

}

object Pixel {

  val BLACK = Pixel(0, 0, 0)
  val white = Pixel(255, 255, 255)
  val RED = Pixel(255, 0, 0)
  val GREEN = Pixel(0, 255, 0)
  val BLUE = Pixel(0, 0, 255)
  val GREY = Pixel(128, 128, 128)

  //clamps a value between 0-255
  def clamp(v: Int): Int =
    if v <= 0 then 0
    else if v > 255 then 255
    else v

  def main(args: Array[String]): Unit = {
    val red = Pixel(255, 0, 0)
    val yellow = Pixel(255, 255, 0)
    yellow.draw(40, 40, "photoscala/src/main/resources/pixels/yellow.jpg")
  }
}
