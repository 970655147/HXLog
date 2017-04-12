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
	public BeanType loadFromJSON(Map<String, Object> obj, Map<String, IdxType> idxMap);
	// for do not initialize 'Object' except 'int, .., String, JSONObject, JSONArray'			add at 2016.06.20
	public BeanType loadFromJSON(Map<String, Object> obj, Map<String, IdxType> idxMap, Set<String> initObjFilter);
	public JSONObject encapJSON(Map<String, IdxType> idxMap, Map<String, IdxType> filterIdxMap);
	// for cycleDectector, 		 add at 2016.06.19
	public JSONObject encapJSON(Map<String, IdxType> idxMap, Map<String, IdxType> filterIdxMap, Deque<Object> cycleDectector);
	
	// 创建一个当前类对象的实例[用于BaseDao]
	public BeanType newInstance(Object... args);
	public String id();
	
	// 获取beanKey, 以及protoBean			add at 2016.06.18
	// 默认的加载方式, 默认的filter
	public String beanKey();
	public BeanType protoBean();
	public IdxType defaultLoadIdx();
	public IdxType defaultFilterIdx();
	public JSONTransferable<BeanType, IdxType> set(String attr, Object val);
	// for add element to 'Collection' 				add at 2016.06.20
	public JSONTransferable<BeanType, IdxType> add(String attr, Object val);
}
