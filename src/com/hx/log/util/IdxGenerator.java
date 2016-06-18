/**
 * file name : IdxGenerator.java
 * created at : 12:19:14 PM May 30, 2016
 * created by 970655147
 */

package com.hx.log.util;

import java.util.concurrent.atomic.AtomicInteger;

// 索引生成工具
public class IdxGenerator {
	
	// 生成索引[线程安全]
	private AtomicInteger idxGenerator = new AtomicInteger();

	// 配置索引start
	public void setIdx(int idx) {
		idxGenerator.set(idx);
	}

	// 生成id
	public int nextId() {
		return idxGenerator.getAndIncrement();
	}
	
}
