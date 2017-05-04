package com.hx.log.io.interf;

/**
 * BuffSizeEstimator
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 12:13 AM
 */
public interface BuffSizeEstimator {

    /**
     * 根据buff阈值获取buffSize的接口
     *
     * @param threshold 给定的阈值
     * @return
     * @author Jerry.X.He
     * @date 5/5/2017 12:13 AM
     * @since 1.0
     */
    int getBuffSize(int threshold);

}
