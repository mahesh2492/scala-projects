package com.learning.photoscala

import scala.annotation.targetName

trait Transformations {
  def apply(image: Image): Image
}

case class Crop(x: Int, y: Int, w: Int, h: Int) extends Transformations {
  override def apply(image: Image): Image = {
    try {
      image.crop(x, y, w, h)
    } catch {
      case _: Exception =>
        println(s"Error: coordinates are out of bounds. Max coordinates: ${image.width} ${image.height}")
        image
    }
  }
}

case object Grayscale extends Transformations {
  override def apply(image: Image): Image = {
    image.map { pixel =>
      val avg = (pixel.r + pixel.g + pixel.b) / 3
      Pixel(avg, avg, avg)
    }
  }
}

case object Invert extends Transformations {
  override def apply(image: Image): Image = {
    image.map { pixel =>
      Pixel(
        255 - pixel.r,
        255 - pixel.g,
        255 - pixel.b
      )
    }
  }
}

case class Colorize(color: Pixel) extends Transformations {
  override def apply(image: Image): Image = {
    image.map { pixel =>
      val avg = (pixel.r + pixel.g + pixel.b) / 3
      Pixel(
        (color.r   * (avg / 255.0)).toInt,
        (color.g * (avg / 255.0)).toInt,
        (color.b  * (avg / 255.0)).toInt
      )

    }
  }
}

case class Blend(fgImage: Image, blendMode: BlendMode) extends Transformations {
  override def apply(bgImage: Image): Image = {
    if(fgImage.width != bgImage.width || fgImage.height != bgImage.height) {
      println(s"Error: images don't have the same sizes: ${fgImage.width} * ${fgImage.height} vs ${bgImage.width} * ${bgImage.height}")
      bgImage
    } else {
      val width = fgImage.width
      val height = fgImage.height
      val result = Image.black(width, height)

      for {
        x <- 0 until width
        y <- 0 until height
      } do result.setColor(
        x,
        y,
        blendMode.combine(
          fgImage.getColor(x, y),
          bgImage.getColor(x, y)
        )
      )

      result
    }
  }
}

case class Window(width: Int, height: Int, pixels: List[Pixel])
case class Kernel(width: Int, height: Int, values: List[Double]) {
  def normalize(): Kernel = {
    val sum = values.sum
    if(sum == 0) this
    else Kernel(width, height, values.map(_ / sum))
  }

  def multiply_v2(window: Window): Pixel = {
    assert(this.width == window.width && this.height == window.height)

    val (red, green, blue) = window.pixels
      .zip(values)
      .map {
        case (Pixel(r, g, b), k) => (r * k, g * k, b * k)
      }
      .reduce {
        case ((r1, g1, b1), (r2, g2, b2)) => (r1 + r2, g1 + g2, b1 + b2)
      }
    Pixel(red.toInt, green.toInt, blue.toInt)
  }
  @targetName("multiply")
  infix def *(window: Window): Pixel = {
    assert(this.width == window.width && this.height == window.height)
    val red = window.pixels
      .map(_.r)
      .zip(values)
      .map { case (r, k) => r * k }
      .sum
      .toInt

    val green = window.pixels
      .map(_.g)
      .zip(values)
      .map { case (g, k) => g * k }
      .sum
      .toInt

    val blue = window.pixels
      .map(_.b)
      .zip(values)
      .map { case (b, k) => b * k }
      .sum
      .toInt

    Pixel(red, green, blue)
  }
}

object Kernel {
  val blur = Kernel(3, 3, List(
    1.0, 2.0, 1.0,
    2.0, 4.0, 2.0,
    1.0, 2.0, 1.0
  )).normalize()

  val sharpen = Kernel(3, 3, List(
    0.0, -1.0, 0.0,
    -1.0, 5.0, -1.0,
    0.0, -1.0,0.0
  )).normalize()

  val edge = Kernel(3, 3, List(
    1.0, 0.0, -1.0,
    2.0, 0.0, -2.0,
    1.0, 0.0, -1.0
  ))

  val emboss = Kernel(3, 3, List(
    -2.0, -1.0, 0.0,
    -1.0, 1.0, 1.0,
    0.0, 1.0, 2.0
  ))

}
case class KernelFilter(kernel: Kernel) extends Transformations {
  override def apply(image: Image): Image = {
    val pixels = for {
      y <- 0 until image.height
      x <- 0 until image.width
    } yield kernel * image.window(x, y, kernel.width, kernel.height)

    Image(image.width, image.height, pixels.toArray)
  }
}


object Transformations {
  def main(args: Array[String]): Unit = {
     val original = Image.loadResource("nebula.jpg")
     val grayscale = Grayscale(original)
     grayscale.saveResources("nebula_gray.jpg")
     Invert(original).saveResources("nebula_negative.jpg")
     Colorize(Pixel.GREEN)(original).saveResources("nebula_green.jpg")

    val nebula = Image.loadResource("nebula_cropped.jpg")
    val satellite = Image.loadResource("blue_marble_cropped.jpg")
    val metal = Image.loadResource("metal_worn_cropped.jpg")
    val multiply = Blend(satellite, Multiply)(nebula)
    val transparency = Blend(satellite, new Transparency(0.3))(nebula)
    val overlay = Blend(satellite, Overlay)(nebula)
    overlay.saveResources("nebula_blue_marble_overlay.jpg")

    val metalWorn = Blend(metal, Overlay)(nebula)
    metalWorn.saveResources("nebula_metal_worn_overlay.jpg")

    val nebulablur = KernelFilter(Kernel.blur)(nebula)
    nebulablur.saveResources("nebula_blur.jpg")

    val nebulaSharpen = KernelFilter(Kernel.sharpen)(nebula)
    nebulaSharpen.saveResources("nebula_sharpen.jpg")

    val nebularEmboss = KernelFilter(Kernel.emboss)(nebula)
    nebularEmboss.saveResources("nebula_emboss.jpg")
  }
}
