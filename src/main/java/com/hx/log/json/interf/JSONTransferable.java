/**
 * file name : JSONTransferable.java
 * created at : 4:52:00 PM May 22, 2016
 * created by 970655147
 */

package com.hx.log.json.interf;

import com.hx.json.JSONObject;
import com.hx.json.config.interf.JSONConfig;

import java.util.Deque;
import java.util.Map;
import java.util.Set;

/**
 * bean 和json字符串之间相互转换的接口
 *
 * @author Jerry.X.He <970655147@qq.com>
 * @version 1.0
 * @date 5/5/2017 8:09 AM
 */
public interface JSONTransferable<BeanType extends JSONTransferable<BeanType>> {

    // 从json字符串到bean之间的转化
    // 从bean到json字符串之间的转换
    // 具体的业务逻辑由开发进行规约, 我这里的idxMap主要的作用在于获取当前采用那一套索引, 过滤器

    /**
     * 从给定的JSONObject以及属性名称映射创建一个Bean返回
     *
     * @param obj           给定的Map
     * @param config        解析json的config
     * @return bean that constructed by obj
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    BeanType loadFromJSON(Map<String, Object> obj, JSONConfig config);

    /**
     * 将当前Bean根据给定的属性索引映射 和需要过滤的属性映射构建一个JSONObject表示
     *
     * @param config 解析json的config
     * @return JSONObject represent currentStartIdx JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    JSONObject encapJSON(JSONConfig config);

    /**
     * 将当前Bean根据给定的属性索引映射 和需要过滤的属性映射构建一个JSONObject表示
     * for cycleDectector, 		 add at 2016.06.19
     *
     * @param config   解析json的config
     * @param cycleDectector 循环检测的Set
     * @return JSONObject represent currentStartIdx JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    JSONObject encapJSON(JSONConfig config, Deque<Object> cycleDectector);

    // 创建一个当前类对象的实例[用于BaseDao]
    BeanType newInstance(Object... args);

    /**
     * 获取当前JSONTransferable的id
     *
     * @return id
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    String id();

    /**
     * 配置当前JSONTransferable的id
     *
     * @return id
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    void id(String id);

}
