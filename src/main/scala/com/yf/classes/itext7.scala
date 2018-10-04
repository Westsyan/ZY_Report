package com.yf.classes

import java.io.File

import com.itextpdf.kernel.colors.DeviceRgb
import com.itextpdf.kernel.font.PdfFontFactory
import com.itextpdf.kernel.pdf.{PdfDocument, PdfWriter}
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.Paragraph
import com.itextpdf.layout.property.TextAlignment
import com.itextpdf.text.pdf.BaseFont
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._
import scala.util.control.Breaks.{break, breakable}

object itext7 {


  def addTextInHisttroy(inpath: String, outpath: String, code: Int, resPath: String, text: Array[String]) = {
    val reader = new com.itextpdf.kernel.pdf.PdfReader(inpath)
    val writer = new PdfWriter(outpath)
    val pdfDoc = new PdfDocument(reader, writer)
    val document = new Document(pdfDoc)
    val width = 523
    val black = new DeviceRgb(62, 62, 62)
    val font = PdfFontFactory.createFont(resPath + "/resource/fonts/msyh.ttc,1", BaseFont.IDENTITY_H, BaseFont.EMBEDDED)
    val font1 = PdfFontFactory.createFont(resPath + "/resource/fonts/msyhbd.ttc,1", BaseFont.IDENTITY_H, BaseFont.EMBEDDED)
    var textHead = 684.25f
    if (!text.isEmpty) {
      text.map { x =>
        val head = new Paragraph("● " + x.split("\n").head).setFont(font1).setFontSize(9).setFontColor(black).setTextAlignment(TextAlignment.LEFT).setFixedPosition(1,36.8496f, textHead, width)
        val histroy = x.split("\n").drop(1).map(_.trim)
        var histroyLines = 0
        for (hl <- histroy) {
          val size = getLines(hl)
          histroyLines = histroyLines + (size / 57).toInt + 1
        }
        val textlines = textHead - 16f * histroyLines

        val body = new Paragraph(histroy.mkString("\n")).setFont(font).setFontSize(9).setFontColor(black).setTextAlignment(TextAlignment.JUSTIFIED).setFixedPosition(1, 36.8496f, textHead - 16f * histroyLines, width)
        textHead = textHead - 16f * histroyLines - 25f
        document.add(head)
        document.add(body)
      }
    }
    val titleHead = textHead + 25f - 54.3f + 22f

    val title = new Paragraph("相关历史名人").setFont(font1).setFontSize(10).setFontColor(new DeviceRgb(40, 181, 116)).setTextAlignment(TextAlignment.LEFT).setFixedPosition(1, 37.3584f, titleHead, width)
    document.add(title)

    val histroy  = FileUtils.readFileToString(new File(resPath + "/resource/text/stroy/" + code + ".txt"),"UTF-8")
    val histroySize = getLines(histroy)
    val moreLine = if(histroySize/(histroySize/58.5).toInt == 58.5){1}else{0}

    val histroyLines = titleHead - 33.1f - 16f*((histroySize/58.5).toInt - moreLine)
    val histroyText = new Paragraph(histroy).setFont(font).setFontSize(9).setFontColor(black).setFixedLeading(16f).setTextAlignment(TextAlignment.JUSTIFIED).setFixedPosition(1, 36.8496f, histroyLines, width)

    document.add(histroyText)

    val famous = FileUtils.readLines(new File(resPath + "/resource/text/famous/" + code + ".txt"),"UTF-8").asScala

    for(i <- 0 until famous.size){
      val famousAll= famous.map(_.split("\t"))
      val famousLine = getLines(famousAll(i).last)
      val x1 =  if((famousLine/(famousLine/14).toInt) == 14.0){0}else{1}
      val famousLines = histroyLines - 174.5f - 35.5f - 16f * ((famousLine/14).toInt -1)
      val famousName = new Paragraph(famousAll(i).head).setFont(font).setFontSize(8).setFontColor(new DeviceRgb(255,255,255)).setTextAlignment(TextAlignment.CENTER).setFixedPosition(1, 36.8496f + 134f*i, histroyLines - 172f, 118f)
      val famousText = new Paragraph(famousAll(i).last).setFont(font).setFontSize(8).setFontColor(black).setFixedLeading(16f).setTextAlignment(TextAlignment.JUSTIFIED).setFixedPosition(1, 36.8496f + 134f*i, famousLines, 118f)
      document.add(famousName)
      document.add(famousText)
    }
    document.close()
    reader.close()
    writer.close()
    pdfDoc.close()
  }

  def addSummarizeToPdf(inpath: String, outpath: String, headHeight: Float, code: Int, resPath: String,info:Array[String]): Array[String] = {
    val reader = new com.itextpdf.kernel.pdf.PdfReader(inpath)
    val writer = new PdfWriter(outpath)
    val pdfDoc = new PdfDocument(reader, writer)
    val document = new Document(pdfDoc)
    val width = 523
    val black = new DeviceRgb(62, 62, 62)
    val font = PdfFontFactory.createFont(resPath + "/resource/fonts/msyh.ttc,1", BaseFont.IDENTITY_H, BaseFont.EMBEDDED)
    val font1 = PdfFontFactory.createFont(resPath + "/resource/fonts/msyhbd.ttc,1", BaseFont.IDENTITY_H, BaseFont.EMBEDDED)
    val name = new Paragraph(info(1)).setFont(font1).setFontSize(9).setFontColor(black).setFixedLeading(16f).setTextAlignment(TextAlignment.LEFT).setFixedPosition(1, 36.8496f, 632f, width)
    document.add(name)
    val line = FileUtils.readLines(new File(resPath + "/resource/configure/line.txt"), "UTF-8").asScala
    val lines = line(code - 1).split("\t")
    val down = new Paragraph(info(2) +"单倍群，它属于 " + info(3) + " 单倍群的下游支系。").setFont(font).setFontSize(9).setFontColor(black).setFixedLeading(16f).setTextAlignment(TextAlignment.LEFT).setFixedPosition(1, 36.8496f, 583.1f, width)
    document.add(down)

    val summarizeTxt = FileUtils.readFileToString(new File(resPath + "/resource/text/summarize/" + code + ".txt"),"UTF-8")
    val y = 495.7f - 16 * (lines.last.toInt - 1)
    val text = new Paragraph(summarizeTxt).setFont(font).setFontSize(9).setFontColor(black).setFixedLeading(16f).setTextAlignment(TextAlignment.JUSTIFIED).setFixedPosition(1, 36.8496f, y, width)
    document.add(text)
    val title = new Paragraph("单倍群 " + lines.head).setFont(font1).setFontSize(10).setFontColor(new DeviceRgb(40, 181, 116)).setTextAlignment(TextAlignment.LEFT).setFixedPosition(1, 37.3584f, 528.8f, width)
    val title2 = new Paragraph("您的父系迁徙路线").setFont(font1).setFontSize(9).setFontColor(new DeviceRgb(40, 181, 116)).setTextAlignment(TextAlignment.LEFT).setFixedPosition(1, 787.1763f, headHeight, width)
    val title3 = new Paragraph("——近1 万年以来主要人群的大致扩散路线").setFont(font1).setFontSize(9).setFontColor(black).setTextAlignment(TextAlignment.LEFT).setFixedPosition(1, 859.1763f, headHeight, width)
    val title4 = new Paragraph("迁徙路线阐释").setFont(font1).setFontSize(10).setFontColor(new DeviceRgb(40, 181, 116)).setTextAlignment(TextAlignment.LEFT).setFixedPosition(1, 649.1353f, headHeight - 41.45f, width)
    document.add(title)
    document.add(title2)
    document.add(title3)
    document.add(title4)
    var textHead = headHeight - 74.75f

    val migraTxt = FileUtils.readFileToString(new File(resPath + "/resource/text/migrate/" + code + ".txt"),"UTF-8")
    var nextPage = new Array[String](0)
    val migraArray = migraTxt.split("bold")
    breakable(
      migraArray.map { x =>
        val head = new Paragraph("● " + x.split("\n").head).setFont(font1).setFontSize(9).setFontColor(black).setTextAlignment(TextAlignment.LEFT).setFixedPosition(1, 649.1353f, textHead, width)
        val histroy = x.split("\n").drop(1).map(_.trim)
        var histroyLines = 0
        for (hl <- histroy) {
          val size = getLines(hl)
          histroyLines = histroyLines + (size / 57).toInt + 1
        }
        val textlines = textHead - 16f * histroyLines
        if (textlines < 45f) {
          nextPage = migraArray.drop(migraArray.indexOf(x))
          break()
        }

        val body = new Paragraph(histroy.mkString("\n")).setFont(font).setFontSize(9).setFontColor(black).setTextAlignment(TextAlignment.JUSTIFIED).setFixedPosition(1, 649.1353f, textHead - 16f * histroyLines, width)
        textHead = textHead - 16f * histroyLines - 25f
        document.add(head)
        document.add(body)
      })
    document.close()
    reader.close()
    writer.close()
    pdfDoc.close()
    nextPage
  }

  def getLines(text:String) : Double = {
    var size = 0.0
    val pa = "^[a-zA-Z0-9-]*$".r
    text.map { z =>
      //计算每行的字符数
      if (pa.findFirstIn(z.toString).isDefined) {
        size = size + 0.5
      } else {
        size = size + 1.0
      }
    }
    size
  }
}
