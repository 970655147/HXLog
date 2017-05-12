package com.hx.log.alogrithm.tree.interf;

import com.hx.json.JSONObject;

import java.io.File;

/**
 * 将给定的文件的信息 封装到给定的结果信息中
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/12/2017 9:36 PM
 */
public interface EncapFileInfo {

    /**
     * 收集给定的file的信息到result中
     *
     * @param file   给定的文件
     * @param result 收集结果的JSONObject
     * @return void
     * @author Jerry.X.He
     * @date 5/12/2017 9:36 PM
     * @since 1.0
     */
    JSONObject encapFileInfo(File file, JSONObject result);

}
