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
      (fg.r * factor + bg.r * (1 - factor)).toInt,
      (fg.g * factor + bg.g * (1 - factor)).toInt,
      (fg.b * factor + bg.b * (1 - factor)).toInt
    )
}

object Multiply extends BlendMode {
  override def combine(fg: Pixel, bg: Pixel): Pixel = {
    Pixel(
      (fg.r * bg.r / 255.0).toInt,
      (fg.g * bg.g / 255.0).toInt,
      (fg.b * bg.b / 255.0).toInt
    )
  }
}

object Screen extends BlendMode {
  override def combine(fg: Pixel, bg: Pixel): Pixel = {
    Pixel(
      (255 - (255 - fg.r) * (255 - bg.r) / 255.0).toInt,
      (255 - (255 - fg.g) * (255 - bg.g) / 255.0).toInt,
      (255 - (255 - fg.b) * (255 - bg.b) / 255.0).toInt
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
      (255 * f(bg.r / 255.0, fg.r / 255.0)).toInt,
      (255 * f(bg.g / 255.0, fg.g / 255.0)).toInt,
      (255 * f(bg.b / 255.0, fg.b / 255.0)).toInt
    )
}