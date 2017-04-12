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

// ʵ��ͼ��ͨ�ŵĹܵ��ӿ�
public class ImageTransferable implements Transferable {
	// �洢��img����
	private RenderedImage img;

	// ��ʼ�� [�Բ� ����ĳ�ʼ������, ���һ�����һЪ, img = img [�ͼ�����] ]
   public ImageTransferable(RenderedImage img) {
	   this.img = img;
   }

   // getTransferDataFlavors
   public DataFlavor[] getTransferDataFlavors() {
      return new DataFlavor[] { DataFlavor.imageFlavor };
   }

   // ������dataFlavor�Ƿ���ϵ�ǰ��ImageTransferable������
   public boolean isDataFlavorSupported(DataFlavor flavor) {
      return flavor.equals(DataFlavor.imageFlavor);
   }

   // ���ش洢������
   public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException {
      if (flavor.equals(DataFlavor.imageFlavor)) {
         return img;
      } else {
         throw new UnsupportedFlavorException(flavor);
      }
   }
}
