/**
 * file name : BufferUtils.java
 * created at : 20:06:51 2016-12-30
 * created by 970655147
 */

package com.hx.log.io;

import com.hx.log.util.Log;
import com.hx.log.util.Tools;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public final class BufferManager {
	
	// ------------ ������� ------- 2016.03.16 -------------
	// ��Ÿ���buffer, �Լ�buffer��Ĭ��ˢ����ֵ��С
	// Ĭ�ϵ�BuffSizeEstimator
	public static int DEFAULT_BUFF_THRESHOLD = 128 << 10;
	public static BuffSizeEstimator DEFAULT_BUFFSIZE_ESTIMATOR = new BuffSizeEstimator() {
		public int getBuffSize(int threshold) {
			return threshold + (threshold >> 3);
		}
	};
	public static BufferHandler DEFAULT_BUFF_HANDLER = new BufferHandler() {
		@Override
		public void beforeHandle(BuffInfo buffInfo) throws Exception {
			
		}
		@Override
		public void handleBuffer(BuffInfo buffInfo) throws Exception {
			flushBuffer(buffInfo.sb, buffInfo.outputPath, buffInfo.charset);
		}
		@Override
		public void afterHandle(BuffInfo buffInfo) throws Exception {
			
		}
	};
	
	/**
	 * buffName -> buffInfo
	 */
	private Map<String, BuffInfo> bufferToBuffInfo = new HashMap<>();
	
	/**
	 * ��ʼ��
	 */
	public BufferManager() {
		
	}
	
	// ��ȡ���еĻ�������key�ļ���
	public Set<String> buffNames() {
		return new HashSet<>(bufferToBuffInfo.keySet() );
	}
	// ����һ��������
	public void createAnBuffer(String bufName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator, BufferHandler handler) {
		Tools.assert0(bufName != null, "'bufName' can't be null ");
		Tools.assert0(outputPath != null, "'outputPath' can't be null ");
		Tools.assert0(charset != null, "'charset' can't be null ");
		Tools.assert0(threshold > 0, "'threshold' must > 0 ");
		Tools.assert0(buffSizeEstimator != null, "'buffSizeEstimator' can't be null ");
		Tools.assert0(handler != null, "'handler' can't be null ");
		
		if(bufExists(bufName) ) {
			throw new RuntimeException("the buffInfo with key : " + bufName + " is already exists !");
		}
		
		BuffInfo buffInfo = new BuffInfo(outputPath, charset, threshold, buffSizeEstimator, handler);
		bufferToBuffInfo.put(bufName, buffInfo);
	}
	public void createAnBuffer(String bufName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator) {
		createAnBuffer(bufName, outputPath, charset, threshold, buffSizeEstimator, DEFAULT_BUFF_HANDLER);
	}
	public void createAnBuffer(String bufName, String outputPath, String charset) {
		createAnBuffer(bufName, outputPath, charset, DEFAULT_BUFF_THRESHOLD, DEFAULT_BUFFSIZE_ESTIMATOR);
	}
	public void createAnBuffer(String bufName, String outputPath) {
		createAnBuffer(bufName, outputPath, Tools.DEFAULT_CHARSET);
	}
	public void createAnBufferIfNotExists(String bufName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator, BufferHandler handler) {
		if(! bufExists(bufName) ) {
			BuffInfo buffInfo = new BuffInfo(outputPath, charset, threshold, buffSizeEstimator, handler);
			bufferToBuffInfo.put(bufName, buffInfo);
		}
	}
	public void createAnBufferIfNotExists(String bufName, String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator) {
		createAnBufferIfNotExists(bufName, outputPath, charset, threshold, buffSizeEstimator, DEFAULT_BUFF_HANDLER);
	}
	public void createAnBufferIfNotExists(String bufName, String outputPath, String charset, BufferHandler handler) {
		createAnBufferIfNotExists(bufName, outputPath, charset, DEFAULT_BUFF_THRESHOLD, DEFAULT_BUFFSIZE_ESTIMATOR, handler);
	}
	public void createAnBufferIfNotExists(String bufName, String outputPath, String charset) {
		createAnBufferIfNotExists(bufName, outputPath, charset, DEFAULT_BUFF_HANDLER);
	}
	public void createAnBufferIfNotExists(String bufName, String outputPath) {
		createAnBufferIfNotExists(bufName, outputPath, Tools.DEFAULT_CHARSET);
	}
	public void closeAnBuffer(String bufName) throws Exception {
		flushBuffer(bufName, true);
	}
	public void closeAllBuffer() throws Exception {
		for(String bufName : buffNames() ) {
			closeAnBuffer(bufName);
		}
	}
	// �жϸ�����bufName��buffer�Ƿ����
	public boolean bufExists(String buffName) {
		return getBuffInfo(buffName) != null;
	}
	public BuffInfo getBuffInfo(String buffName) {
		return bufferToBuffInfo.get(buffName);
	}
	
	// ������Ļ�������������� �����buffer�е������Ƿ񳬹�����ֵ
	public void appendBuffer(String bufName, String content, boolean appendCRLF) throws Exception {
		Tools.assert0(bufName != null, "'bufName' can't be null ");
		if(! bufExists(bufName)) {
			throw new RuntimeException("have no buffInfo with key : " + bufName + ", please createAnBuffer first !");
		}
		
		BuffInfo buffInfo = bufferToBuffInfo.get(bufName);
		buffInfo.sb.append(content);
		if(appendCRLF) {
			buffInfo.sb.append(Tools.CRLF);
		}
		if(buffInfo.sb.length() >= buffInfo.threshold) {
			buffInfo.handler.beforeHandle(buffInfo);
			synchronized(buffInfo.sb) {
				if(buffInfo.sb.length() >= buffInfo.threshold) {
					// judge if 'buf' exists in case of 'MultiThreadConcurrent'
					if(bufExists(bufName) ) {
//						flushBuffer(buffInfo.sb, buffInfo.outputPath, buffInfo.charset, logFlags);
						buffInfo.handler.handleBuffer(buffInfo);
					} else {
						Log.log("the buffer : '" + bufName + "' already be removed !");
					}
				}
			}
			buffInfo.handler.afterHandle(buffInfo);
		}
	}
	public void appendBuffer(String bufName, String content) throws Exception {
		appendBuffer(bufName, content, false);
	}
	public void appendBufferCRLF(String bufName, String content) throws Exception {
		appendBuffer(bufName, content, true);
	}
	
	// ˢ�����������
	public void flushBuffer(String bufName, boolean isLastBatch) throws Exception {
		Tools.assert0(bufName != null, "'bufName' can't be null ");
		if(! bufExists(bufName)) {
			throw new RuntimeException("have no buffInfo with key : " + bufName + ", please createAnBuffer first !");
		}
		
		BuffInfo buffInfo = bufferToBuffInfo.get(bufName);
		if(buffInfo.sb.length() > 0) {
			buffInfo.handler.beforeHandle(buffInfo);
			synchronized (buffInfo.sb) {
				if(buffInfo.sb.length() > 0) {
					// judge if 'buf' exists in case of 'MultiThreadConcurrent'
					if(bufExists(bufName) ) {
//						flushBuffer(buffInfo.sb, buffInfo.outputPath, buffInfo.charset, logFlags);
						buffInfo.handler.handleBuffer(buffInfo);
						
						if(isLastBatch) {
							bufferToBuffInfo.remove(bufName);
						}
					} else {
						Log.log("the buffer : '" + bufName + "' already be removed !");
					}
				}
			}
			buffInfo.handler.afterHandle(buffInfo);
		}
	}
	public void flushBuffer(String bufName) throws Exception {
		flushBuffer(bufName, false);
	}
	
	// update the step 'flushDataToPath' into 'threadPoolExecutor'		at 2016.04.16
	public static void flushBuffer(final StringBuffer sb, final String path, final String charset) throws IOException {
		Tools.assert0(sb != null, "'sb' can't be null ");
		Tools.assert0(path != null, "'path' can't be null ");
		Tools.assert0(charset != null, "'charset' can't be null ");
		
		// move 'nextThree' a head incase of 'buff.sb.length >= buff.threshold', got an circle, but can't clear 'buff.sb'		at 2016.04.23
		long kbLength = Tools.getKBytesByBytes(sb.length() );
		String content = sb.toString();
		sb.setLength(0);
		
		if(! Tools.threadPool.isShutdown() ) {
			Tools.append(content, path, charset, true);
		} else {
			Tools.append(content, path, charset, false);
		}
	}
	public void flushBuffer(StringBuffer sb, String path) throws IOException {
		flushBuffer(sb, path, Tools.DEFAULT_CHARSET);
	}
	
	
	/**
	 * -------------------------------------------------------- assist datastructures -------------------------------------------------
	 */
	
	// ��Ż�����Ϣ
	public static class BuffInfo {
		// ���·��, ˢ�����ݵ���ֵ, �����С, StringBuffer
		public String outputPath;
		public String charset;
		public int threshold;
		public int buffSize;
		public StringBuffer sb;
		public BufferHandler handler;
		
		// ��ʼ��
		public BuffInfo(String outputPath, String charset, int threshold, BuffSizeEstimator buffSizeEstimator, BufferHandler handler) {
			this.outputPath = outputPath;
			this.charset = charset;
			this.threshold = threshold;
			this.buffSize = buffSizeEstimator.getBuffSize(threshold);
			this.handler = handler;
			this.sb = new StringBuffer(buffSize);
		}
	}
	// ����buff��ֵ��ȡbuffSize�Ľӿ�
	public static interface BuffSizeEstimator {
		public int getBuffSize(int threshold);
	}
	// 'BufferHandler'	 add at 2016.06.04
	public static interface BufferHandler {
		public void beforeHandle(BuffInfo buffInfo) throws Exception;
		// must flush in 'synchronizedBlock'
		public void handleBuffer(BuffInfo buffInfo) throws Exception;
		public void afterHandle(BuffInfo buffInfo) throws Exception;
	}
	
}
