/**
 * file name : IdxIterator.java
 * created at : ����9:47:52 2016��8��11��
 * created by 970655147
 */

package com.hx.log.idx.interf;

// һ����ȡ�����ĵ�����
public interface IdxIterator {

	/**
	 * �Ƿ�����һ������
	 *
	 * @return  true if next invoke of next() return idx, or else
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:17 PM
	 * @since 1.0
	 */
	boolean hasNext();

	/**
	 * ��ȡ��һ������
	 *
	 * @return  next index
	 * @author Jerry.X.He
	 * @date 4/12/2017 10:17 PM
	 * @since 1.0
	 */
	int next();
	
}