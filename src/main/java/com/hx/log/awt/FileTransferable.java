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

// ʵ���ļ�ͨ�ŵĹܵ��ӿ�
public class FileTransferable implements Transferable {
	// �洢��file����
	private List<File> files;

	// ��ʼ�� [�Բ� ����ĳ�ʼ������, ���һ�����һЪ, img = img [�ͼ�����] ]
   public FileTransferable(List<File> files) {
	   this.files = files;
   }

   // getTransferDataFlavors
   public DataFlavor[] getTransferDataFlavors() {
      return new DataFlavor[] { DataFlavor.javaFileListFlavor };
   }

   // ������dataFlavor�Ƿ���ϵ�ǰ��ImageTransferable������
   public boolean isDataFlavorSupported(DataFlavor flavor) {
      return flavor.equals(DataFlavor.javaFileListFlavor);
   }

   // ���ش洢������
   public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException {
      if (flavor.equals(DataFlavor.javaFileListFlavor)) {
         return files;
      } else {
         throw new UnsupportedFlavorException(flavor);
      }
   }
}
