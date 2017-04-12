/**
 * file name : FileUtils.java
 * created at : 22:02:05 2016-12-30
 * created by 970655147
 */

package com.hx.log.file;

import com.hx.log.util.Tools;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

public final class FileUtils {

	/**
	 * 默认的charset, writeAsync
	 */
	public static String DEFAULT_CHARSET = Tools.DEFAULT_CHARSET;
	public static boolean WRITE_ASYNC = Tools.WRITE_ASYNC;
	
	// disable constructor
	private FileUtils() {
		Tools.assert0("can't instantiate !");
	}

	// ----------------- 文件操作相关方法 -----------------------
	// 判断是否需要打印日志
	public static boolean isLog(long logFlags, long logMask) {
		return ((logFlags & logMask) != 0);
	}
	// 将html字符串保存到指定的文件中
	// add 'isAsync' at 2016.04.16
	public static void save(String html, File targetFile, String charset, boolean isAsync) throws IOException {
		write(html, targetFile, charset, isAsync, false);
	}
	public static void save(String html, File nextTmpFile, String charset) throws IOException {
		save(html, nextTmpFile, charset, WRITE_ASYNC);
	}
	public static void save(String html, File nextTmpFile, boolean isAsync) throws IOException {
		save(html, nextTmpFile, DEFAULT_CHARSET, isAsync);
	}
	public static void save(String html, File nextTmpFile) throws IOException {
		save(html, nextTmpFile, DEFAULT_CHARSET, WRITE_ASYNC);
	}
	public static void save(String html, String nextTmpName, String charset, boolean isAsync) throws IOException {
		save(html, new File(nextTmpName), charset, isAsync);
	}
	public static void save(String html, String nextTmpName, String charset) throws IOException {
		save(html, nextTmpName, charset, WRITE_ASYNC );
	}
	public static void save(String html, String nextTmpName, boolean isAsync) throws IOException {
		save(html, nextTmpName, DEFAULT_CHARSET, isAsync);
	}
	public static void save(String html, String nextTmpName) throws IOException {
		save(html, nextTmpName, DEFAULT_CHARSET, WRITE_ASYNC );
	}
	
	public static void append(String html, File nextTmpFile, String charset, boolean isAsync) throws IOException {
		write(html, nextTmpFile, charset, isAsync, true);
	}
	public static void append(String html, File nextTmpFile, String charset) throws IOException {
		append(html, nextTmpFile, charset, WRITE_ASYNC);
	}
	public static void append(String html, File nextTmpFile, boolean isAsync) throws IOException {
		append(html, nextTmpFile, DEFAULT_CHARSET, isAsync);
	}
	public static void append(String html, File nextTmpFile) throws IOException {
		append(html, nextTmpFile, DEFAULT_CHARSET, WRITE_ASYNC);
	}
	public static void append(String html, String nextTmpName, String charset, boolean isAsync) throws IOException {
		append(html, new File(nextTmpName), charset, isAsync);
	}
	public static void append(String html, String nextTmpName, String charset) throws IOException {
		append(html, nextTmpName, charset, WRITE_ASYNC );
	}	
	public static void append(String html, String nextTmpName, boolean isAsync) throws IOException {
		append(html, nextTmpName, DEFAULT_CHARSET, isAsync);
	}
	public static void append(String html, String nextTmpName) throws IOException {
		append(html, nextTmpName, DEFAULT_CHARSET, WRITE_ASYNC );
	}
	
	// 1. could use 'tryWithResource' replace 'tryFinally'
	// 2. update 'BufferedOutputStream' with 'FileOutputStream' cause there need not 'Buffer'
	// at 2016.04.16
	public static void write(final String html, final File targetFile, final String charset, boolean isAsync, final boolean isAppend) throws IOException {
		Tools.assert0(html != null, "'html' can't be null ");
		Tools.assert0(targetFile != null, "'targetFile' can't be null ");
		Tools.assert0(charset != null, "'charset' can't be null ");
		
		Runnable writeTask = (new Runnable() {
			@Override
			public void run() {
				try (FileOutputStream fos = new FileOutputStream(targetFile, isAppend) ) {
					fos.write(html.getBytes(charset) );
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		});
		
		if(! isAsync) {
			writeTask.run();
		} else {
			Tools.execute(writeTask);
		}
	}
	public static void write(final String html, final File nextTmpFile, final String charset, final boolean isAppend) throws IOException {
		write(html, nextTmpFile, charset, WRITE_ASYNC, isAppend);
	}
	public static void write(final String html, final File nextTmpFile, final boolean isAppend) throws IOException {
		write(html, nextTmpFile, DEFAULT_CHARSET, WRITE_ASYNC, isAppend);
	}
	
	// 移除指定的文件
	public static void delete(String path) {
		Tools.assert0(path != null, "'path' can't be null ");
		
		File file = new File(path);
		if(file.exists() ) {
			boolean isSucc = file.delete();
		}
	}
	
    // 复制指定的文件
    public static void copy(String src, String dst) throws IOException {
    	Tools.assert0(src != null, "'src' can't be null ");
    	Tools.assert0(dst != null, "'dst' can't be null ");
    	
        File srcFile = new File(src);
        File dstFile = new File(dst);
        if(srcFile.isDirectory() ) {
            return ;
        }
        if(! srcFile.exists() ) {
            return ;
        }
        if(dstFile.exists() ) {
            return ;
        }

        FileInputStream fis = new FileInputStream(srcFile);
        FileOutputStream fos = new FileOutputStream(dstFile);
        copy(fis, fos);
    }

	// 获取给定的输入流中的字符内容
	public static String getContent(InputStream is, String charset) throws IOException {
		Tools.assert0(is != null, "'inputStream' can't be null ");
		Tools.assert0(charset != null, "'charset' can't be null ");
		
		StringBuilder sb = new StringBuilder(is.available() );
		try (BufferedReader br = new BufferedReader(new InputStreamReader(is, charset)) ) {
			String line = null;
			while((line = br.readLine()) != null) {
				sb.append(line );
				sb.append(Tools.CRLF);
			}
		}
		
		return sb.toString();
	}
	public static String getContent(InputStream is) throws IOException {
		return getContent(is, DEFAULT_CHARSET);
	}
	public static String getContent(String path, String charset) throws IOException {
		return getContent(new File(path), charset);
	}
	public static String getContent(File file, String charset) throws IOException {
		return getContent(new FileInputStream(file), charset);
	}
	public static String getContent(String path) throws IOException {
		return getContent(new File(path), DEFAULT_CHARSET);
	}
	public static String getContent(File file) throws IOException {
		return getContent(file, DEFAULT_CHARSET);
	}
	
	// 获取文件的所有的行, 存储在一个结果的List, 文件过大, 慎用此方法
	public static List<String> getContentWithList(File file, String charset, int estimateSize) throws IOException {
		Tools.assert0(file != null, "'file' can't be null ");
		Tools.assert0(charset != null, "'charset' can't be null ");
		
		List<String> lines = new ArrayList<>(estimateSize);
		try (BufferedReader br = new BufferedReader(new InputStreamReader(new FileInputStream(file), charset)) ) {
			String line = null;
			while((line = br.readLine()) != null) {
				lines.add(line);
			}
		}
		
		return lines;
	}
	public static List<String> getContentWithList(File file, int estimateSize) throws IOException {
		return getContentWithList(file, DEFAULT_CHARSET, estimateSize);
	}
	public static List<String> getContentWithList(String file, String charset, int estimateSize) throws IOException {
		return getContentWithList(new File(file), charset, estimateSize);
	}
	public static List<String> getContentWithList(String file, int estimateSize) throws IOException {
		return getContentWithList(new File(file), estimateSize);
	}
	public static List<String> getContentWithList(File file, String charset) throws IOException {
		return getContentWithList(file, charset, Tools.ESTIMATE_FILE_LINES);
	}
	public static List<String> getContentWithList(File file) throws IOException {
		return getContentWithList(file, DEFAULT_CHARSET);
	}
	public static List<String> getContentWithList(String file, String charset) throws IOException {
		return getContentWithList(new File(file), charset );
	}
	public static List<String> getContentWithList(String file) throws IOException {
		return getContentWithList(new File(file) );
	}
	
	
	// 从指定的url上面下载图片  保存到指定的路径下面 [也适用于下载其他的二进制数据]
	public static void downloadFrom(String urlStr, String path) throws IOException {
		Tools.assert0(urlStr != null, "'urlStr' can't be null ");
		Tools.assert0(path != null, "'path' can't be null ");
		
		URL url = new URL(urlStr);
		InputStream is = url.openStream();
		OutputStream os = new FileOutputStream(new File(path));
		copy(is,  os);
	}
	
	// 将输入流中的数据 复制到输出流
	public static void copy(InputStream is, OutputStream os, boolean isCloseStream) {
		Tools.assert0(is != null, "'inputStream' can't be null ");
		Tools.assert0(os != null, "'outputStream' can't be null ");
		
		BufferedInputStream bis = null;
		BufferedOutputStream bos = null;
		try {
			bis = new BufferedInputStream(is);
			bos = new BufferedOutputStream(os);
			int len = 0;
			byte[] buf = new byte[Tools.BUFF_SIZE_ON_TRANS_STREAM];
			while((len = bis.read(buf)) != -1) {
				bos.write(buf, 0, len);
			}
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if(isCloseStream) {
				if(bos != null) {
					try {
						bos.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
				if(bis != null) {
					try {
						bis.close();
					} catch (IOException e) {
						e.printStackTrace();
					}
				}
			}
		}
	}
	public static void copy(InputStream is, OutputStream os) {
		copy(is, os, true);
	}
	
	// 获取指定路径的文件的文件, 通过sep分割的文件名     获取文件名
	// 解析? 的位置, 是为了防止一下情况
	public static String getFileName(String path, char sep) {
		Tools.assert0(path != null, "'path' can't be null ");
		int start = path.lastIndexOf(sep) + 1;
		
//		http://webd.home.news.cn/1.gif?z=1&_wdxid=01002005057000300000000001110
		int end = getSymAfterFileName(path, start+1);
		if(end != -1) {
			return path.substring(start, end);
		} else {
			return path.substring(start);
		}
	}
	// 获取文件名后面的可能出现的符合的最近的索引
	private static int getSymAfterFileName(String path, int start) {
		int min = -1;
		for(int i=start; i<path.length(); i++) {
			if(Tools.MAYBE_FILE_NAME_SEPS.contains(path.charAt(i)) ) {
				min = i;
				break ;
			}
		}
		
		return min;
	}
	
}
