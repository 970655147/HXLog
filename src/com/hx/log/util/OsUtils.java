/**
 * file name : OsUtils.java
 * created at : 23:11:59 2016-12-30
 * created by 970655147
 */

package com.hx.log.util;

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.image.RenderedImage;
import java.io.File;
import java.io.IOException;
import java.util.List;

public final class OsUtils {

	// disable constructor
	private OsUtils() {
		Tools.assert0("can't instantiate !");
	}
	
	
	// ------------ �����ݸ��Ƶ����а� ------- 2016.04.07 -------------
	// windows���а� ���ڴ潻������
	public static void copyStringToClipBoard(String str) {
//		Clipboard clipboard = System.getToolkit().getSystemClipboard();
		Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
		StringSelection ss = new StringSelection(str);
		clipboard.setContents(ss, null);
	}
	public static void copyImgToClipBoard(RenderedImage img) {
      Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard(); //�õ�ϵͳ������
      ImageTransferable selection = new ImageTransferable(img);  //ͼ��ͨ��
      clipboard.setContents(selection, null);
    }
	public static void copyFilesToClipBoard(List<File> files) {
      Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard(); 
      FileTransferable selection = new FileTransferable(files );  
      clipboard.setContents(selection, null);
    }	
	public static String getStringFromClipBoard(){
		Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
		DataFlavor flavor = DataFlavor.stringFlavor;
		String res = Tools.EMPTY_STR;
		
		if(clipboard.isDataFlavorAvailable(flavor)){ //�Ƿ���ϼ��������������
			try {
				res = clipboard.getData(flavor).toString();
			} catch (UnsupportedFlavorException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		return res;
	}
	public static RenderedImage getImgFromClipBoard() {
		Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
		DataFlavor flavor = DataFlavor.imageFlavor;  
		RenderedImage img = null;
		      
		if (clipboard.isDataFlavorAvailable(flavor)) {
		   try {
		  	 img = (RenderedImage) clipboard.getData(flavor);
		   } catch (UnsupportedFlavorException e) {
		  	 e.printStackTrace();
		   } catch (IOException e) {
		      e.printStackTrace();
		   }
		}
	  
		return img;
   }
   public static List<File> getFilesFromClipBoard() {
      Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
      DataFlavor flavor = DataFlavor.javaFileListFlavor;  
      List<File> files = null;
      
      if (clipboard.isDataFlavorAvailable(flavor)) {
         try {
        	 files = (List<File>) clipboard.getData(flavor);
         } catch (UnsupportedFlavorException e) {
        	 e.printStackTrace();
         } catch (IOException e) {
            e.printStackTrace();
         }
      }
      
      return files;
   }
	
}
