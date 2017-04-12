/**
 * file name : TmpGetter.java
 * created at : 9:31:46 PM Nov 24, 2015
 * created by 970655147
 */

package com.hx.log.file;

import java.util.concurrent.atomic.AtomicInteger;

// 获取临时文件相关的数据
public class TmpGetter {
	
    // 临时文件夹, 临时文件名, 临时文件的索引, 临时文件的后缀
	public String tmpDir;
	public String tmpName;
	public final AtomicInteger tmpIdx;
	public String suffix;
	
	// 初始化
	public TmpGetter(String tmpDir, String tmpName, int tmpIdx, String suffix) {
		this.tmpDir = tmpDir;
		this.tmpName = tmpName;
		this.tmpIdx = new AtomicInteger(tmpIdx);
		this.suffix = suffix;
	}
	
	// 获取下一个临时文件路径
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
	// 获取临时文件夹相关
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

	// 获取下一个临时文件名
	protected String getNextTmpName() {
		return tmpName + (String.valueOf(tmpIdx.getAndIncrement()) );
	}
}
