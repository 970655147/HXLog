/**
 * file name : ImageFravable.java
 * created at : 9:18:42 PM Oct 23, 2015
 * created by 970655147
 */

package com.hx.log.awt;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.image.RenderedImage;

// 实现图像通信的管道接口
public class ImageTransferable implements Transferable {
	// 存储的img对象
	private RenderedImage img;

	// 初始化 [卧槽 这里的初始化问题, 把我还看了一歇, img = img [低级错误] ]
   public ImageTransferable(RenderedImage img) {
	   this.img = img;
   }

   // getTransferDataFlavors
   public DataFlavor[] getTransferDataFlavors() {
      return new DataFlavor[] { DataFlavor.imageFlavor };
   }

   // 给定的dataFlavor是否符合当前的ImageTransferable的条件
   public boolean isDataFlavorSupported(DataFlavor flavor) {
      return flavor.equals(DataFlavor.imageFlavor);
   }

   // 返回存储的数据
   public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException {
      if (flavor.equals(DataFlavor.imageFlavor)) {
         return img;
      } else {
         throw new UnsupportedFlavorException(flavor);
      }
   }
}
