/**
 * file name : TmpGetter.java
 * created at : 9:31:46 PM Nov 24, 2015
 * created by 970655147
 */

package com.hx.log.file;

import java.util.concurrent.atomic.AtomicInteger;

// ��ȡ��ʱ�ļ���ص�����
public class TmpGetter {
	
    // ��ʱ�ļ���, ��ʱ�ļ���, ��ʱ�ļ�������, ��ʱ�ļ��ĺ�׺
	public String tmpDir;
	public String tmpName;
	public final AtomicInteger tmpIdx;
	public String suffix;
	
	// ��ʼ��
	public TmpGetter(String tmpDir, String tmpName, int tmpIdx, String suffix) {
		this.tmpDir = tmpDir;
		this.tmpName = tmpName;
		this.tmpIdx = new AtomicInteger(tmpIdx);
		this.suffix = suffix;
	}
	
	// ��ȡ��һ����ʱ�ļ�·��
	public String getNextTmpPath() {
		return tmpDir + "\\" + getNextTmpName() + suffix;
	}
	public String getNextTmpPath(String suffix) {
		return tmpDir + "\\" + getNextTmpName() + suffix;
	}
	public String getNextTmpPath(String fileName, String suffix) {
		return tmpDir + "\\" + fileName + suffix;
	}
	public String getTmpPath(int idx) {
		return tmpDir + "\\" + tmpName + idx + suffix;
	}
	public String getTmpPath(int idx, String suffix) {
		return tmpDir + "\\" + tmpName + idx + suffix;
	}
	public String getTmpPath(String name) {
		return tmpDir + "\\" + name + suffix;
	}
	public String getTmpPath(String name, String suffix) {
		return tmpDir + "\\" + name + suffix;
	}
	// ��ȡ��ʱ�ļ������
	public String getNextTmpDir() {
		return tmpDir + "\\" + getNextTmpName();
	}
	public String getTmpDir(int idx) {
		return tmpDir + "\\" + tmpName + idx;
	}
	public String getTmpDir(String name) {
		return tmpDir + "\\" + name;
	}
	
	// setter
	public void setTmpIdx(int tmpIdx) {
		this.tmpIdx.set(tmpIdx);
	}
	public void setTmpDir(String tmpDir) {
		this.tmpDir = tmpDir;
	}
	public void setTmpName(String tmpName) {
		this.tmpName = tmpName;
	}
	public void setSuffix(String suffix) {
		this.suffix = suffix;
	}

	// ��ȡ��һ����ʱ�ļ���
	protected String getNextTmpName() {
		return tmpName + (String.valueOf(tmpIdx.getAndIncrement()) );
	}
}
