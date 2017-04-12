/**
 * file name : StringIntercepter.java
 * created at : 10:47:13 AM Oct 1, 2015
 * created by 970655147
 */

package com.hx.log.str;

import com.hx.log.util.Tools;

// ��ȡ�ַ����Ĺ���
public class StringInterceptor {
	// Ŀ���ַ���, �Լ���ǰ������
	private String val;
	private int idx;
	
	// ��ʼ��
	public StringInterceptor(String val) {
		this.val = val;
		idx = 0;
	}
	
	// ��������Ŀ���ַ���, ����������
	public void setVal(String val) {
		this.val = val;
		resetIdx();
	}
	
	// ��������
	public void resetIdx(){
		setIdx(0);
	}
	// ��������λ��
	public void setIdx(int idx) {
		this.idx = idx;
	}
	// ��ȡ����λ��
	public int getIdx() {
		return this.idx;
	}
	
	// �Ƿ񻹴�����һ��Ԫ��??
	public boolean hasNext() {
		return idx < val.length();
	}
	
	// ��ȡ��һ��starts, ends֮����ַ���, includeStarts, includeEnds��ʾ����Ƿ����starts, ends
	// ���Ȼ�ȡ��һ��start��λ��, ���û���ҵ�, ֱ�ӷ���""
	// ��ȡstartIdx֮�����һ��ends��λ��, ���û���ҵ�, ֱ�ӷ���""
	// ����Ƿ���Ҫ����starts, ends, ��������, ��ȡ�Ӵ�
	public String nextStrInRange(String starts, String ends, boolean includeStarts, boolean includeEnds) {
		int startIdx = val.indexOf(starts, idx);
		if(startIdx == -1) {
			idx = val.length();
			return Tools.EMPTY_STR;
		}

		int endIdx = val.indexOf(ends, startIdx + starts.length());
		if(endIdx == -1) {
			idx = val.length();
			return Tools.EMPTY_STR;
		}

		if(! includeStarts) {
			startIdx += starts.length();
		}
		if(includeEnds) {
			endIdx += ends.length();
		}
		idx = endIdx + ends.length();
		return val.substring(startIdx, endIdx);
	}
	// ��ȡ��һ��starts, ends֮����ַ���, Ĭ�ϲ�����starts, ends
	public String nextStrInRange(String starts, String ends) {
		return nextStrInRange(starts, ends, false, false);
	}

	// ��ȡ����һ��idxStr֮����ַ���, includeIdx��ʾ����Ƿ����idxStr
	// ���Ȼ�ȡ��һ��idxStr��λ��, ���û���ҵ�, ֱ�ӷ���""
	// ����Ƿ���Ҫ����starts, ends, ��������, ��ȡ�Ӵ�
	public String nextStrInRange(String idxStr, boolean includeIdx) {
		int idxStrIdx = val.indexOf(idxStr, idx);
		if(idxStrIdx == -1) {
			idx = val.length();
			return Tools.EMPTY_STR;
		}
		
		if(includeIdx) {
			idxStrIdx += idxStr.length();
		}
		int oldIdx = idx;
		idx = idxStrIdx + idxStr.length();
		return val.substring(oldIdx, idxStrIdx);
	}
	// ��ȡ����һ��idxStr֮����ַ���, Ĭ�ϲ�����idxStr
	public String nextStrInRange(String idxStr) {
		return nextStrInRange(idxStr, false);
	}

}
