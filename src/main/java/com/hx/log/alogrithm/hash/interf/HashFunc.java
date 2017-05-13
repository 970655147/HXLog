package com.hx.log.alogrithm.hash.interf;

/**
 * һ��Hash�����ĳ���
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:11 PM
 */
public interface HashFunc<T> {

    /**
     * ��������Ķ����hash
     *
     * @param obj �����Ķ���
     * @return the hash caclculated
     * @author Jerry.X.He
     * @date 4/12/2017 9:12 PM
     * @since 1.0
     */
    int hash(T obj);

}
