package com.yf.classes

import java.io.{File, FileOutputStream}
import javax.imageio.ImageIO

import com.itextpdf.text.BaseColor
import com.itextpdf.text.pdf.{PdfReader, PdfStamper}
import com.yf.classes.itext7.getLines
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._

object addElementToPdf {

  def addFamoustoPdf(inPath:String,outPath:String,resPath:String,code:Int,text:Array[String]) = {
    val reader = new PdfReader(inPath)
    val stamper = new PdfStamper(reader, new FileOutputStream(outPath))
    val over = stamper.getOverContent(1)
    var textHead = 684.25f
    if (!text.isEmpty) {
      text.map { x =>
        val histroy = x.split("\n").drop(1).map(_.trim)
        var histroyLines = 0
        for (hl <- histroy) {
          val size = getLines(hl)
          histroyLines = histroyLines + (size / 57).toInt + 1
        }
        val textlines = textHead - 16f * histroyLines
        textHead = textHead - 16f * histroyLines - 25f
      }
    }
    val titleHead = textHead + 25f - 54.3f + 22f
    val histroy  = FileUtils.readFileToString(new File(resPath + "/resource/text/stroy/" + code + ".txt"),"UTF-8")
    val histroySize = getLines(histroy)

    val moreLine = if(histroySize/(histroySize/58.5).toInt == 58.5){1}else{0}

    val histroyLines = titleHead - 33.1f - 16f*((histroySize/58.5).toInt - moreLine)

    val nums = FileUtils.readLines(new File(resPath + "/resource/text/famous/" + code + ".txt"),"UTF-8").asScala
    itext5.addFullLine(over,37.5f,558.95f,titleHead + 24f,new BaseColor(62,62,62))
    itext5.addDashLine(over,37.5f,558.95f,titleHead - 7f,new BaseColor(62,62,62))
    for( i <- 0 until  nums.size) {
      itext5.addImageToPdf(over, resPath + "/resource/picture/famous/" + code + "_" + (i + 1) + ".png", 37.42f + 134f * i, histroyLines-174.5f, 118f, 161.5f)
      itext5.addRectangleWithBackgroundColor(over, 37.42f + 134f * i, histroyLines-174.5f+18.75f, 37.42f + 118f + 134f*i, histroyLines - 174.5f, new BaseColor(62, 62, 63))
    }

    stamper.close()
    reader.close()
  }


  def add(inPath:String,outPath:String,resPath:String,code:Int) : Float = {
    val reader = new PdfReader(inPath)
    val stamper = new PdfStamper(reader, new FileOutputStream(outPath))
    val over = stamper.getOverContent(1)
    val line = FileUtils.readLines(new File(resPath + "/resource/configure/line.txt"),"UTF-8").asScala
    val lines = line(code - 1).split("\t").last
    val migraFold = getFold(resPath + "/resource/picture/migrate/" + code +"_1.png")
    val migraHeight = 406f - (lines.toInt - 3) * 16f
    val migraWidth = migraHeight / migraFold
    val migraLeft = (603.78f - migraWidth)/2
    itext5.addImageToPdf(over,resPath + "/resource/picture/migrate/"+code+"_1.png",migraLeft,60f,migraWidth,migraHeight)
    val diffFold = getFold(resPath + "/resource/picture/migrate/" + code +"_2.png")
    val diffWidth = migraWidth
    val diffHeight = diffWidth*diffFold
    val diffLeft = 603.78f + (603.78f-diffWidth)/2
    itext5.addImageToPdf(over,resPath + "/resource/picture/migrate/" + code + "_2.png",diffLeft,698.8f-diffHeight,diffWidth,diffHeight)
    val headHeight = 698.8f-diffHeight
    itext5.addFullLine(over,649f,1170.71f,headHeight - 35.4f,new BaseColor(62,62,62))
    itext5.addDashLine(over,649f,1170.71f,headHeight - 67.2f,new BaseColor(62,62,62))
    stamper.close()
    reader.close()

    headHeight
  }


  def getFold(path:String) : Float = {
    val image = ImageIO.read(new File(path))
    val imageWidth = image.getWidth.toFloat
    val imageHeight = image.getHeight.toFloat
    val fold = imageHeight/imageWidth
    fold
  }
}
