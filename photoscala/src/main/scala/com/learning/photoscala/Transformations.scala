package com.learning.photoscala

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
      val avg = (pixel.red + pixel.green + pixel.blue) / 3
      Pixel(avg, avg, avg)
    }
  }
}

case object Invert extends Transformations {
  override def apply(image: Image): Image = {
    image.map { pixel =>
      Pixel(
        255 - pixel.red,
        255 - pixel.green,
        255 - pixel.blue
      )
    }
  }
}

case class Colorize(color: Pixel) extends Transformations {
  override def apply(image: Image): Image = {
    image.map { pixel =>
      val avg = (pixel.red + pixel.green + pixel.blue) / 3
      Pixel(
        (color.red   * (avg / 255.0)).toInt,
        (color.green * (avg / 255.0)).toInt,
        (color.blue  * (avg / 255.0)).toInt
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
  }
}
