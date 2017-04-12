/**
 * file name : IdxGenerator.java
 * created at : 12:19:14 PM May 30, 2016
 * created by 970655147
 */

package com.hx.log.idx;

import java.util.concurrent.atomic.AtomicInteger;

// �������ɹ���
public class IdxGenerator {
	// ���Ĭ��ֵ
	public static final int DEFAULT_INIT = 0;
	public static final int DEFAULT_STEP = 1;
	
	// ��������[�̰߳�ȫ]
	private AtomicInteger idxGenerator = new AtomicInteger();
	private int step;
	
	// ��ʼ��
	public IdxGenerator() {
		this(DEFAULT_INIT, DEFAULT_STEP);
	}
	public IdxGenerator(int init) {
		this(init, DEFAULT_STEP);
	}
	public IdxGenerator(int init, int step) {
		idx(init);
		this.step = step;
	}

	// ��������start
	public void idx(int idx) {
		idxGenerator.set(idx);
	}
	public void step(int step) {
		this.step = step;
	}
	public int idx() {
		return idxGenerator.get();
	}
	public int step() {
		return step;
	}
	
	// ����id
	public int nextId() {
		return idxGenerator.getAndAdd(step);
	}
	
}
