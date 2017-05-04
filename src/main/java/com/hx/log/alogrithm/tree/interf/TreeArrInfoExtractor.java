package com.hx.log.alogrithm.tree.interf;

import com.hx.json.JSONObject;

/**
 * TreeArrInfoExtractor
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 10:25 PM
 */
public interface TreeArrInfoExtractor {

    /**
     * 获取Array的信息, 用字符串表示
     *
     * @param obj 给定的Array的元数据的封装
     * @return
     * @author Jerry.X.He
     * @date 5/4/2017 10:24 PM
     * @since 1.0
     */
    String getTreeArrInfo(JSONObject obj);

}
