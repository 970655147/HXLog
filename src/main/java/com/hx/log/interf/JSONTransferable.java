/**
 * file name : JSONTransferable.java
 * created at : 4:52:00 PM May 22, 2016
 * created by 970655147
 */

package com.hx.log.interf;

import java.util.Deque;
import java.util.Map;
import java.util.Set;

import net.sf.json.JSONObject;

// bean 和json字符串之间相互转换的接口
public interface JSONTransferable<BeanType extends JSONTransferable<BeanType, IdxType>, IdxType> {

    // 从json字符串到bean之间的转化
    // 从bean到json字符串之间的转换
    // 具体的业务逻辑由开发进行规约, 我这里的idxMap主要的作用在于获取当前采用那一套索引, 过滤器

    /**
     * 从给定的JSONObject以及属性名称映射创建一个Bean返回
     *
     * @param obj    给定的Map
     * @param idxMap 给定的属性名称映射
     * @return bean that constructed by obj
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    BeanType loadFromJSON(Map<String, Object> obj, Map<String, IdxType> idxMap);

    // for do not initialize 'Object' except 'int, .., String, JSONObject, JSONArray'			add at 2016.06.20

    /**
     * 从给定的JSONObject以及属性名称映射创建一个Bean返回
     *
     * @param obj           给定的Map
     * @param idxMap        给定的属性名称映射
     * @param initObjFilter 不需要递归loadFromJSON的属性
     * @return bean that constructed by obj
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    BeanType loadFromJSON(Map<String, Object> obj, Map<String, IdxType> idxMap, Set<String> initObjFilter);

    /**
     * 将当前Bean根据给定的属性索引映射 和需要过滤的属性映射构建一个JSONObject表示
     *
     * @param idxMap       给定的Map
     * @param filterIdxMap 给定的属性名称映射
     * @return JSONObject represent current JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    JSONObject encapJSON(Map<String, IdxType> idxMap, Map<String, IdxType> filterIdxMap);

    // for cycleDectector, 		 add at 2016.06.19

    /**
     * 将当前Bean根据给定的属性索引映射 和需要过滤的属性映射构建一个JSONObject表示
     *
     * @param idxMap         给定的Map
     * @param filterIdxMap   给定的属性名称映射
     * @param cycleDectector 循环检测的Set
     * @return JSONObject represent current JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:23 PM
     * @since 1.0
     */
    JSONObject encapJSON(Map<String, IdxType> idxMap, Map<String, IdxType> filterIdxMap, Deque<Object> cycleDectector);

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

    // 获取beanKey, 以及protoBean			add at 2016.06.18
    // 默认的加载方式, 默认的filter

    /**
     * 获取当前类JSONTransferable的beanKey, 向idxMap, filterIdxMap注册
     *
     * @return beanKey
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    String beanKey();

    /**
     * 获取当前类JSONTransferable的一个Dummy对象
     *
     * @return dummy of JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    BeanType protoBean();

    /**
     * 获取当前类JSONTransferable的默认idxMap的idx
     *
     * @return dummy of JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    IdxType defaultLoadIdx();

    /**
     * 获取当前类JSONTransferable的默认filterIdxMap的idx
     *
     * @return dummy of JSONTransferable
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    IdxType defaultFilterIdx();

    /**
     * 配置当前JSONTransferable的给定的属性的值
     *
     * @param attr 给定的属性的名称
     * @param val  给定的属性的值
     * @return set `attr`'s value to be `val`
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    JSONTransferable<BeanType, IdxType> set(String attr, Object val);

    // for add element to 'Collection' 				add at 2016.06.20
    /**
     * 为集合属性添加一个数据
     *
     * @param attr 给定的属性的名称
     * @param val  给定的属性的值
     * @return set `attr`'s value to be `val`
     * @author Jerry.X.He
     * @date 4/12/2017 10:28 PM
     * @since 1.0
     */
    JSONTransferable<BeanType, IdxType> add(String attr, Object val);
}
