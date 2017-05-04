package com.hx.log.alogrithm.tree.interf;

import com.hx.json.JSONObject;

/**
 * TreeObjInfoExtractor
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/4/2017 10:24 PM
 */
public interface TreeObjInfoExtractor {

    /**
     * 获取对象的信息, 用字符串表示
     *
     * @param obj 给定的对象信息的封装
     * @return
     * @author Jerry.X.He
     * @date 5/4/2017 10:24 PM
     * @since 1.0
     */
    String getTreeObjInfo(JSONObject obj);

}
