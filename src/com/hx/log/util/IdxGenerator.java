/**
 * file name : IdxGenerator.java
 * created at : 12:19:14 PM May 30, 2016
 * created by 970655147
 */

package com.hx.log.util;

import java.util.concurrent.atomic.AtomicInteger;

// �������ɹ���
public class IdxGenerator {
	
	// ��������[�̰߳�ȫ]
	private AtomicInteger idxGenerator = new AtomicInteger();

	// ��������start
	public void setIdx(int idx) {
		idxGenerator.set(idx);
	}

	// ����id
	public int nextId() {
		return idxGenerator.getAndIncrement();
	}
	
}
