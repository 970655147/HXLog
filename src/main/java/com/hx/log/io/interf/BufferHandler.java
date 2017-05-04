package com.hx.log.io.interf;

import com.hx.log.io.BuffInfo;
import com.hx.log.io.BufferManager;

/**
 * BufferHandler
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 12:14 AM
 */
// 'BufferHandler'	 add at 2016.06.04
public interface BufferHandler {

    /**
     * 输出buffer之前需要处理的业务
     *
     * @param buffInfo 给定的buffInfo
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:14 AM
     * @since 1.0
     */
    void beforeHandle(BuffInfo buffInfo) throws Exception;

    /**
     * 输出buffer, must flush in 'synchronizedBlock'
     *
     * @param buffInfo 给定的buffInfo
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:14 AM
     * @since 1.0
     */
    void handleBuffer(BuffInfo buffInfo) throws Exception;

    /**
     * 输出buffer之后需要处理的业务
     *
     * @param buffInfo 给定的buffInfo
     * @return void
     * @author Jerry.X.He
     * @date 5/5/2017 12:14 AM
     * @since 1.0
     */
    void afterHandle(BuffInfo buffInfo) throws Exception;

}
