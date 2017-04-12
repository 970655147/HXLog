/**
 * file name : ImageFravable.java
 * created at : 9:18:42 PM Oct 23, 2015
 * created by 970655147
 */

package com.hx.log.awt;

import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.io.File;
import java.util.List;

// 实现文件通信的管道接口
public class FileTransferable implements Transferable {
	// 存储的file对象
	private List<File> files;

	// 初始化 [卧槽 这里的初始化问题, 把我还看了一歇, img = img [低级错误] ]
   public FileTransferable(List<File> files) {
	   this.files = files;
   }

   // getTransferDataFlavors
   public DataFlavor[] getTransferDataFlavors() {
      return new DataFlavor[] { DataFlavor.javaFileListFlavor };
   }

   // 给定的dataFlavor是否符合当前的ImageTransferable的条件
   public boolean isDataFlavorSupported(DataFlavor flavor) {
      return flavor.equals(DataFlavor.javaFileListFlavor);
   }

   // 返回存储的数据
   public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException {
      if (flavor.equals(DataFlavor.javaFileListFlavor)) {
         return files;
      } else {
         throw new UnsupportedFlavorException(flavor);
      }
   }
}
