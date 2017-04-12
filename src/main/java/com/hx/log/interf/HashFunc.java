package com.hx.log.interf;

/**
 * 一个Hash函数的抽象
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 4/12/2017 9:11 PM
 */
public interface HashFunc {

    /**
     * 计算给定的对象的hash
     *
     * @param obj 给定的对象
     * @return
     * @author Jerry.X.He
     * @date 4/12/2017 9:12 PM
     * @since 1.0
     */
    int hash(Object obj);

}
