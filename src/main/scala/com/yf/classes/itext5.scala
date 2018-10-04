package com.yf.classes

import java.io.{ByteArrayOutputStream, File, FileOutputStream, IOException}
import java.util

import com.itextpdf.text.pdf._
import com.itextpdf.text.pdf.parser.{ImageRenderInfo, PdfReaderContentParser, RenderListener, TextRenderInfo}
import com.itextpdf.text._
import org.apache.commons.io.FileUtils

import scala.collection.JavaConverters._

object itext5 {


  def main(args: Array[String]): Unit = {
    val x = FileUtils.readLines(new File("D:/fandi/ZYreport/test.txt"),"UTF-8").asScala
    val x1 = x.head.split("\t").drop(4)
    val map = new util.HashMap[String, Object]()
    for(i <- 0 until x1.size){
      map.put((i+1).toString,x1(i))
    }
    val o = new util.HashMap[String, Object]()
    o.put("datemap", map)

    pdfoutWithText(o,"D:/fandi/ZYreport/bin/resource/fonts/times.ttf","D:/fandi/4.pdf","D:/fandi/5.pdf")


  }

  //合并pdf文件
  def mergePdfFiles(files: Array[String], savepath: String): Unit = {
    try {
      val read = new PdfReader(files(0))
      val document = new Document(read.getPageSize(1))
      val copy = new PdfCopy(document, new FileOutputStream(savepath))
      document.open()
      for (i <- 0 until files.size) {
        val reader = new PdfReader(files(i))
        val n = reader.getNumberOfPages
        for (j <- 1 to n) {
          document.newPage
          val page = copy.getImportedPage(reader, j)
          copy.addPage(page)
        }
        reader.close()
      }
      document.close()
      read.close()
    } catch {
      case e: IOException =>
        e.printStackTrace()
      case e: DocumentException =>
        e.printStackTrace()
    }
  }

  // 利用模板生成pdf
  def pdfoutWithText(o: util.Map[String, AnyRef],  fontPath: String,inpath:String,outpath:String): Unit = { // 模板路径

    var reader: PdfReader = null
    var out: FileOutputStream = null
    var bos: ByteArrayOutputStream = null
    var stamper: PdfStamper = null
    val canvas: PdfContentByte = null
    try {
      val bf: BaseFont = BaseFont.createFont(fontPath, BaseFont.IDENTITY_H, BaseFont.EMBEDDED)
      //   Font FontChinese = new Font(bf, 5, Font.NORMAL);
      reader = new PdfReader(inpath) // 读取pdf模板
      bos = new ByteArrayOutputStream
      stamper = new PdfStamper(reader, bos)
      val form: AcroFields = stamper.getAcroFields
      //文字类的内容处理
      val datemap: util.Map[String, String] = o.get("datemap").asInstanceOf[util.Map[String, String]]
      form.addSubstitutionFont(bf)
      import scala.collection.JavaConversions._
      for (key <- datemap.keySet) {
        val value: String = datemap.get(key)
        form.setField(key, value)
      }
      stamper.setFormFlattening(true) // 如果为false，生成的PDF文件可以编辑，如果为true，生成的PDF文件不可以编辑
      stamper.close()
      val doc: Document = new Document
      out = new FileOutputStream(outpath) // 输出流
      val copy: PdfCopy = new PdfCopy(doc, out)
      doc.open()
      var importPage: PdfImportedPage = null
      importPage = copy.getImportedPage(new PdfReader(bos.toByteArray), 1)
      copy.addPage(importPage)
      doc.close()
      reader.close()
    } catch {
      case e: IOException =>
        System.out.println(e)
      case e: DocumentException =>
        System.out.println(e)
    }
  }

  /**
    * 插入图片
    *
    * @param picturePath : 图片路径
    * @param x           : x轴坐标（左下角）
    * @param y           : y轴坐标（左下角）
    *                    param ratio  :缩放比例
    */
  def addImageToPdf(over: PdfContentByte, picturePath: String, x: Float, y: Float, width: Float, height: Float): Unit = {
    val image = Image.getInstance(picturePath)
    image.setAbsolutePosition(x, y)
    // image.scalePercent(ratio)
    image.scaleAbsolute(width, height)
    over.addImage(image)
    over.stroke()
  }

  /**
    * 实线
    */
  def addFullLine(over: PdfContentByte, headX: Float, lastX: Float, y: Float, color: BaseColor): Unit = {
    over.setColorStroke(color)
    over.setLineDash(1f)
    over.setLineWidth(0.3f)
    over.moveTo(headX, y)
    over.lineTo(lastX, y)
    over.stroke()
  }

  /**
    * 虚线
    */
  def addDashLine(over: PdfContentByte, headX: Float, lastX: Float, y: Float, color: BaseColor) = {
    over.setColorStroke(color)
    over.setLineDash(2f, 0.1f)
    over.setLineWidth(0.3f)
    over.moveTo(headX, y)
    over.lineTo(lastX, y)
    over.stroke()
  }

  /**
    * 有背景色矩形
    */
  def addRectangleWithBackgroundColor(over: PdfContentByte, headX: Float, headY: Float, lastX: Float, lastY: Float, color: BaseColor) = {
    val rectangle = new Rectangle(headX, headY, lastX, lastY)
    rectangle.setBackgroundColor(color)
    over.rectangle(rectangle)
    over.stroke()
  }

  /**
    * 无背景色矩形框
    *
    * @param width : 矩形边框宽度
    */
  def addRectangleWithoutBackgroundColor(over: PdfContentByte, headX: Float, headY: Float, lastX: Float, lastY: Float, width: Float, color: BaseColor) = {
    over.setColorStroke(color)
    over.setLineDash(1f)
    over.setLineWidth(width)
    over.rectangle(headX, headY, lastX, lastY)
    over.stroke()
  }

  /**
    * 圆角矩形
    *
    * @param radius ：圆角半径
    *               setColorFill:   设置填充颜色为专色
    *               setColorStroke : 设置边框颜色为专色
    *               fillStroke : 使用非零圈数规则填充路径，以确定要填充和描边的区域
    */
  def addRoundRectangle(over: PdfContentByte, headX: Float, headY: Float, lastX: Float, lastY: Float, radius: Float, color: BaseColor) = {
    over.setColorFill(color)
    over.setColorStroke(color)
    over.roundRectangle(headX, headY, lastX, lastY, radius)
    over.fillStroke()
  }

  def getKeyWords(file: String, keyword: String, index: Int): Unit = {
    val pdfReader = new PdfReader(file)
    val pdfReaderContentParser = new PdfReaderContentParser(pdfReader)

    var x: Float = 0
    var y: Float = 0
    pdfReaderContentParser.processContent(1, new RenderListener {
      override def renderImage(renderInfo: ImageRenderInfo) = {
        if (keyword == "image") {
          x = renderInfo.getStartPoint.get(index * 2 - 2)
          y = renderInfo.getStartPoint.get(index * 2 - 1)
        }
      }

      override def endTextBlock(): Unit = {}

      override def renderText(renderInfo: TextRenderInfo) = {
        val text = renderInfo.getText
        if (text != null && text.contains(keyword) && keyword != "image") {
          val boundingRectange = renderInfo.getBaseline.getBoundingRectange
          x = boundingRectange.x
          y = boundingRectange.y
        }
      }

      override def beginTextBlock(): Unit = {}
    })
    println(x + "," + y)
  }

}
