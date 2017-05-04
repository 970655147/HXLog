package com.hx.log.biz.interf;

/**
 * GetLengthStrHandler
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 11:03 PM
 */
public interface GetLengthStrHandler {

    /**
     * 根据数字和单位获取字符串表示的接口
     *
     * @param length 需要计算的单位的量
     * @param dimen  给定的单位
     * @return
     * @author Jerry.X.He
     * @date 5/4/2017 11:03 PM
     * @since 1.0
     */
    String getLengthStr(long length, String dimen);

}
