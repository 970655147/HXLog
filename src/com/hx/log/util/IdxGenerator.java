/**
 * file name : IdxGenerator.java
 * created at : 12:19:14 PM May 30, 2016
 * created by 970655147
 */

package com.hx.log.util;

import java.util.concurrent.atomic.AtomicInteger;

// 索引生成工具
public class IdxGenerator {
	// 相关默认值
	public static final int DEFAULT_INIT = 0;
	public static final int DEFAULT_STEP = 1;
	
	// 生成索引[线程安全]
	private AtomicInteger idxGenerator = new AtomicInteger();
	private int step;
	
	// 初始化
	public IdxGenerator() {
		this(DEFAULT_INIT, DEFAULT_STEP);
	}
	public IdxGenerator(int init) {
		this(init, DEFAULT_STEP);
	}
	public IdxGenerator(int init, int step) {
		setIdx(init);
		this.step = step;
	}

	// 配置索引start
	public void setIdx(int idx) {
		idxGenerator.set(idx);
	}

	// 生成id
	public int nextId() {
		return idxGenerator.getAndAdd(step);
	}
	
}
