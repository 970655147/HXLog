package com.hx.log.interf;

/**
 * Result
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 8:42 PM
 */
public interface Result {

    /**
     * ��ǰ����Ƿ�ɹ�
     *
     * @return the status binding on current result
     * @author Jerry.X.He
     * @date 5/3/2017 8:43 PM
     * @since 1.0
     */
    boolean success();

    /**
     * �����Ӧ��
     *
     * @return the code binding on current result
     * @author Jerry.X.He
     * @date 5/3/2017 8:43 PM
     * @since 1.0
     */
    int code();

    /**
     * �����Ϣ
     *
     * @return the msg binding on current result
     * @author Jerry.X.He
     * @date 5/3/2017 8:43 PM
     * @since 1.0
     */
    String msg();

    /**
     * ��ǰ���������
     *
     * @return the data binding on current result
     * @author Jerry.X.He
     * @date 5/3/2017 8:43 PM
     * @since 1.0
     */
    Object data();

    /**
     * ��ǰ����Ķ�������
     *
     * @return the extra data binding on current result
     * @author Jerry.X.He
     * @date 5/3/2017 8:43 PM
     * @since 1.0
     */
    Object extra();

}
