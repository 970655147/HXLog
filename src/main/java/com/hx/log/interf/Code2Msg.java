package com.hx.log.interf;

/**
 * code -> msg
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/3/2017 8:37 PM
 */
public interface Code2Msg<K, V> {

    /**
     * ��ȡcode
     *
     * @return code binding on current obj
     * @author Jerry.X.He
     * @date 5/3/2017 8:38 PM
     * @since 1.0
     */
    K code();

    /**
     * ��ȡmgs
     *
     * @return msg binding on current obj
     * @author Jerry.X.He
     * @date 5/3/2017 8:38 PM
     * @since 1.0
     */
    V msg();

}
