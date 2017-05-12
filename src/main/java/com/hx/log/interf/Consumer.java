package com.hx.log.interf;

/**
 * �����ߵĳ���
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/12/2017 10:07 PM
 */
public interface Consumer<I, O> {

    /**
     * ���ĸ���������
     *
     * @param content ����������
     * @return void
     * @author Jerry.X.He
     * @date 5/12/2017 9:11 PM
     * @since 1.0
     */
    void consume(I content);

    /**
     * ��ȡ��ǰ�����ߵĽ��
     *
     * @return T the result of consumer
     * @author Jerry.X.He
     * @date 5/12/2017 9:11 PM
     * @since 1.0
     */
    O get();

}
