/**
 * file name : Validateable.java
 * created at : ����5:03:10 2016��8��25��
 * created by 970655147
 */

package com.hx.log.interf;

// Validateable, �о� ������ǰ���Ǹ�Validator����['HXBlog' in 'BaseAction']
public interface Validatable {


	/**
	 * У������Ĳ���, �ж��Ƿ�Ϸ�
	 *
	 * @param args �����б�
	 * @return true if args is valid, or esle
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:19 PM
	 * @since 1.0
	 */
	boolean validate(Object... args);

	/**
	 * ��ȡУ�����֮��, ���ڵ��쳣����
	 *
	 * @return object represent Exception Object after last check
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:19 PM
	 * @since 1.0
	 */
	Object getLastErr();
	
}
