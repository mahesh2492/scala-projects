package com.learning.photoscala

trait BlendMode {
  def combine(fg: Pixel, bg: Pixel): Pixel
}

class Transparency(f: Double) extends BlendMode {
  private val factor = {
    if(f < 0) 0.0
    else if(f > 1) 1.0
    else f
  }

  override def combine(fg: Pixel, bg: Pixel): Pixel =
    Pixel(
      (fg.red * factor + bg.red * (1 - factor)).toInt,
      (fg.green * factor + bg.green * (1 - factor)).toInt,
      (fg.blue * factor + bg.blue * (1 - factor)).toInt
    )
}

object Multiply extends BlendMode {
  override def combine(fg: Pixel, bg: Pixel): Pixel = {
    Pixel(
      (fg.red * bg.red / 255.0).toInt,
      (fg.green * bg.green / 255.0).toInt,
      (fg.blue * bg.blue / 255.0).toInt
    )
  }
}

object Screen extends BlendMode {
  override def combine(fg: Pixel, bg: Pixel): Pixel = {
    Pixel(
      (255 - (255 - fg.red) * (255 - bg.red) / 255.0).toInt,
      (255 - (255 - fg.green) * (255 - bg.green) / 255.0).toInt,
      (255 - (255 - fg.blue) * (255 - bg.blue) / 255.0).toInt
    )
  }
}

object Overlay extends BlendMode {
  private def f(a: Double, b: Double): Double = {
    if(a < 0.5) 2 * a * b
    else
      1 - 2 * (1 - a) * (1 - b)
  }
  override def combine(fg: Pixel, bg: Pixel): Pixel =
    Pixel(
      (255 * f(bg.red / 255.0, fg.red / 255.0)).toInt,
      (255 * f(bg.green / 255.0, fg.green / 255.0)).toInt,
      (255 * f(bg.blue / 255.0, fg.blue / 255.0)).toInt
    )
}