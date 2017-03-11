/**
 * file name : Validateable.java
 * created at : ����5:03:10 2016��8��25��
 * created by 970655147
 */

package com.hx.log.util.interf;

// Validateable, �о� ������ǰ���Ǹ�Validator����['HXBlog' in 'BaseAction']
public interface Validatable {

	// У�鵱ǰ���� �Լ� �����Ĳ���
	// ��ȡ�����װ����
	public boolean validate(Object... args);
	public Object getLastErr();
	
}
